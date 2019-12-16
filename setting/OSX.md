Installing packages for OSX

### Issue
In OsX, python-glm cannot be installed by pip3.
You need to manually install it

csv is already installed. 

## Step 3. 

Install standard packages for all users.
```bash
sudo pip3 install jupyter numpy scipy matplotlib sklearn statsmodels patsy seaborn pandas pydot Pillow  
```


### Install python-glmnet from source

1. Install gfortran from [here](http://gcc.gnu.org/wiki/GFortranBinaries#MacOS)

2. Install python-glmnet

```bash
git clone https://github.com/civisanalytics/python-glmnet.git
cd python-glmnet
python3 setup.py install --user
```
