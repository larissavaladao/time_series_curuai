/**
 * @name Mapbiomas User Toolkit - Altered to include only the target ROI and collections - Table Dissolve Version + CSV
 * @description Exports images and area calculation (CSV) based on dissolved geometry.
 */

var palettes = require('users/mapbiomas/modules:Palettes.js');

// --- 1. AREA CALCULATION MODULE ---
var Area = {
    convert2table: function (obj) {
        var classesAndAreas = ee.List(ee.Dictionary(obj).get('groups'));
        return classesAndAreas.map(function (item) {
            var dict = ee.Dictionary(item);
            return ee.Feature(null, { 
                'class': dict.get('class'), 
                'area_km2': dict.get('sum') 
            });
        });
    },
    calculate: function (object) {
        var reducer = ee.Reducer.sum().group(1, 'class').group(1, 'territory');
        var pixelArea = ee.Image.pixelArea().divide(object.factor);
        
        var territotiesData = pixelArea.addBands(object.territory).addBands(object.image)
            .reduceRegion({
                reducer: reducer,
                geometry: object.geometry,
                scale: object.scale,
                maxPixels: 1e13
            });
            
        var groups = ee.List(territotiesData.get('groups'));
        var areaStats = groups.map(function(g) {
          return Area.convert2table(g);
        }).flatten();
        
        return ee.FeatureCollection(areaStats);
    }
};

// --- 2. MAIN APP ---
var App = {
    options: {
        version: '1.34.5',
        className: { 1: "Forest", 2: "Natural Forest", 3: "Forest Formation", 4: "Savanna Formation", 5: "Magrove", 9: "Forest Plantation", 10: "Non Forest Natural Formation", 11: "Wetland", 12: "Grassland", 13: "Other Non Forest", 14: "Farming", 15: "Pasture", 18: "Agriculture", 19: "Temporary Crops", 20: "Sugar Cane", 21: "Mosaic of Agriculture and Pasture", 22: "Non vegetated area", 23: "Beach and Dune", 24: "Urban Infrastructure", 25: "Other Non Vegetated", 26: "Water", 27: "Non Observed", 29: "Rocky outcrop", 30: "Mining", 31: "Aquaculture", 32: "Salt flat", 33: "River, Lake and Ocean", 39: "Soy Beans", 40: "Rice", 41: "Mosaic of Crops", 46: 'Coffee', 47: 'Citrus', 48: 'Other Perennial', 49: 'Wooded Sandbank', 50: 'Herbaceous Sandbank', 62: "Cotton" },
        tables: { 
            'mapbiomas-brazil': [
                { 'label': 'Curuai - Regional Watershed', 'value': 'projects/ee-curuai2/assets/bacia_regional' },
                { 'label': 'Curuai - Local Watershed', 'value': 'projects/ee-curuai2/assets/bacia_local' }
            ] 
        },
        collections: {
            'mapbiomas-brazil': {
                'collection-10.0': {
                    'assets': { 'integration': 'projects/mapbiomas-public/assets/brazil/lulc/collection10/mapbiomas_brazil_collection10_coverage_v2' },
                    'periods': { 'Coverage': Array.from({length: 40}, function(_, i) { return (1985 + i).toString(); }) }
                }
            }
        },
        dataType: 'Coverage',
        activeTable: null,
        dissolvedGeom: null,
        selectedTableName: ''
    },

    init: function () { this.ui.init(); },

    ui: {
        loadCollectionList: function (regionName) {
            var colOptions = App.options.collections[regionName];
            var select = ui.Select({
                items: Object.keys(colOptions).reverse(),
                placeholder: 'select collection',
                onChange: function (collectioName) {
                    var assets = colOptions[collectioName].assets;
                    App.options.dataImage = ee.Image(assets.integration);
                    App.ui.loadTablesList(regionName);
                },
                style: { stretch: 'horizontal' }
            });
            App.ui.form.panelCollection.widgets().set(1, select);
        },

        loadTablesList: function (regionName) {
            var items = App.options.tables[regionName];
            var select = ui.Select({
                placeholder: 'select table',
                items: items.map(function(obj) { return {label: obj.label, value: obj.value}; }),
                onChange: function (tablePath) {
                    App.options.selectedTableName = items.filter(function(i){return i.value == tablePath})[0].label;
                    var fc = ee.FeatureCollection(tablePath);
                    App.options.dissolvedGeom = fc.geometry().dissolve();
                    App.options.activeTable = fc;

                    Map.clear();
                    Map.addLayer(App.options.dissolvedGeom, {color: 'red'}, 'Area Boundary');
                    Map.centerObject(App.options.dissolvedGeom);
                    App.ui.loadYearList();
                },
                style: { stretch: 'horizontal' }
            });
            App.ui.form.panelTables.widgets().set(1, select);
        },

        loadYearList: function() {
            var years = App.options.collections['mapbiomas-brazil']['collection-10.0'].periods.Coverage;
            App.ui.form.panelLayersList.clear();
            years.forEach(function(year) {
                App.ui.form.panelLayersList.add(ui.Checkbox({
                    label: year,
                    value: false,
                    style: {stretch: 'horizontal', fontSize: '12px', padding: '0px'}
                }));
            });
        },

        exportData: function() {
            var geom = App.options.dissolvedGeom;
            var tableName = App.options.selectedTableName;
            var widgets = App.ui.form.panelLayersList.widgets();
            
            // Create territory mask for area calculation (value 1 inside the dissolved geometry)
            var territory = ee.Image().paint(App.options.activeTable, 1);
            
            var allYearsStats = ee.FeatureCollection([]);

            for (var i = 0; i < widgets.length(); i++) {
                var cb = widgets.get(i);
                if (cb.getValue()) {
                    var year = cb.getLabel();
                    var img = App.options.dataImage.select('classification_' + year);
                    
                    var name = tableName.toLowerCase().indexOf('local') !== -1 ? 
                               'mapbiomas_loc_' + year : 'mapbiomas_reg_' + year;
                    
                    // 1. Export Image
                    Export.image.toDrive({
                        image: img.clip(geom),
                        description: name,
                        folder: 'GEE',
                        fileNamePrefix: name,
                        region: geom.bounds().buffer(100),
                        crs: 'EPSG:32621',
                        scale: 30,
                        maxPixels: 1e13
                    });

                    // 2. Calculate Area for this year
                    var yearArea = Area.calculate({
                        "image": img,
                        "territory": territory,
                        "geometry": geom,
                        "scale": 30,
                        "factor": 1000000, // m2 to km2
                    });

                    // Map class names and add year metadata
                    yearArea = yearArea.map(function(f) {
                        var classId = f.get('class');
                        var className = ee.Dictionary(App.options.className).get(ee.String(classId), "Unknown");
                        return f.set({
                            'year': year,
                            'class_name': className,
                            'watershed': tableName
                        });
                    });
                    
                    allYearsStats = allYearsStats.merge(yearArea);
                }
            }

            // 3. Export CSV Table
            var csvName = tableName.toLowerCase().indexOf('local') !== -1 ? 
                          'area_stats_loc' : 'area_stats_reg';
                          
            Export.table.toDrive({
                collection: allYearsStats,
                description: csvName,
                folder: 'GEE',
                fileNamePrefix: csvName,
                fileFormat: 'CSV'
            });

            print('Exports started! Check the Tasks tab for Images and the CSV.');
        },

        init: function () { this.form.init(); },

        form: {
            init: function () {
                this.panelMain.add(this.labelTitle).add(this.panel1);
                ui.root.add(this.panelMain);
                this.panel1.add(this.panelRegion).add(this.panelCollection).add(this.panelTables)
                           .add(ui.Label('4. Select Years to Export')).add(this.panelLayersList)
                           .add(this.buttonExport);
                App.ui.loadCollectionList('mapbiomas-brazil');
            },
            panelMain: ui.Panel({ style: { width: '320px' }}),
            panel1: ui.Panel(),
            labelTitle: ui.Label('MapBiomas Dissolve Toolkit', {fontWeight: 'bold', fontSize: '18px'}),
            panelRegion: ui.Panel([ui.Label('1. Region'), ui.Select({items: ['mapbiomas-brazil'], value: 'mapbiomas-brazil'})]),
            panelCollection: ui.Panel([ui.Label('2. Collection')]),
            panelTables: ui.Panel([ui.Label('3. Choose Watershed Table')]),
            panelLayersList: ui.Panel({style: {height: '200px', backgroundColor: '#f0f0f0', border: '1px solid #ccc'}}),
            buttonExport: ui.Button({ label: 'Export Images & Areas', onClick: function() { App.ui.exportData(); }, style: {stretch: 'horizontal'}})
        }
    }
};

App.init();