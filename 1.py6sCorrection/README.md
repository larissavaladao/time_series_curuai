Instructions to run atmospheric correction
(@Dinha explicar o processo de como colocar a imagem que vc fez que eu vou colocar na pasta DOCKER para que seguindo os outros passos que voce me passou de tudo certo)

git clone https://github.com/gee-community/ee-jupyter-contrib/

Run the codes below in the atmcorr-ee folder from the repository:

docker build . -t atmcorr-ee

docker run -i -t -p 8888:8888 atmcorr-ee

gcloud auth login

git clone https://github.com/samsammurphy/gee-atmcorr-S2

git clone https://github.com/larissavaladao/py6s_harmonize_sample.git

cd py6s_harmonize_sample/

mv 1.hamonization_py6sCorrection ../gee-atmcorr-S2

cd ../gee-atmcorr-S2/1.hamonization_py6sCorrection

jupyter-notebook py6s_correction.ipynb --ip='*' --port=8888 --allow-root
