import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


def prepare_time_series_plot(paths_var, paths_trend, dict_var, data_dir, color_palette=None):
    """Read paired time-series and trend CSVs and plot them efficiently.

    Parameters
    - paths_var: list of CSV filepaths containing time series (must contain 'time_start' and variables)
    - paths_trend: list of CSV filepaths with trend info; must contain a 'variable' column and a 'trend' column
    - dict_var: mapping variable -> {'title':..., 'label':...}
    - data_dir: base directory (used only for path context)
    - color_palette: optional list of colors for water periods
    """
    if color_palette is None:
        color_palette = ['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728']

    for p_var, p_trend in zip(paths_var, paths_trend):
        df = pd.read_csv(p_var, parse_dates=['time_start']).sort_values('time_start')

        # load trends robustly
        try:
            df_trend = pd.read_csv(p_trend)
        except Exception:
            df_trend = pd.read_csv(p_trend, encoding='utf-8', engine='python')

        if 'Unnamed: 0' in df_trend.columns:
            df_trend = df_trend.drop(columns=['Unnamed: 0'])
        if 'variable' in df_trend.columns:
            df_trend = df_trend.set_index('variable')

        axes = df_trend.index.tolist()

        fig, axs = plt.subplots(len(axes), 1, figsize=(12, 4 * max(1, len(axes))), squeeze=False)

        for i, var in enumerate(axes):
            ax = axs[i, 0]

            if var not in df.columns:
                ax.text(0.5, 0.5, f"{var} not in {os.path.basename(p_var)}", ha='center')
                continue

            series = df[var]
            x = df['time_start']

            ax.plot(x, series, color='gray', linestyle='-.', linewidth=1, alpha=0.6)

            # linear fit (use ordinal x)
            x_ord = x.map(pd.Timestamp.toordinal)
            valid = series.notna() & x_ord.notna()
            if valid.sum() > 1:
                m, b = np.polyfit(x_ord[valid], series[valid], 1)
                ax.plot(x, m * x_ord + b, color='black', linestyle='--', linewidth=1)
            else:
                m = b = np.nan

            # water periods plotting
            if 'water_period' in df.columns:
                water_periods = sorted(df['water_period'].dropna().unique())
                colors = {period: color_palette[i % len(color_palette)] for i, period in enumerate(water_periods)}
                for wp in water_periods:
                    sub = df[df['water_period'] == wp]
                    ax.plot(sub['time_start'], sub[var], marker='o', linestyle='none', markersize=6, color=colors[wp], alpha=0.8, label=str(wp))

            ax.set_xlabel('Date')
            ax.set_ylabel(dict_var.get(var, {}).get('label', var))
            ax.set_title(dict_var.get(var, {}).get('title', var) + ' vs Time')

            # trend annotation if available
            trend_text = ''
            try:
                trend_text = df_trend.loc[var, 'trend']
            except Exception:
                trend_text = ''

            if not np.isnan(m):
                ax.annotate(f"y = {m:.3f}x + {b:.3f}\n{trend_text}", xy=(0.75, 1.01), xycoords='axes fraction')

            ax.legend()

        plt.tight_layout()
        plt.show()
        plt.close(fig)
