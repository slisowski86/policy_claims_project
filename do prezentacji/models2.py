# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.genmod.generalized_estimating_equations import GEE
from statsmodels.genmod.cov_struct import (Exchangeable,
    Independence,Autoregressive)
from statsmodels.genmod.families import Poisson

fam = Poisson()
ind = Independence()
data_model.exog = pd.read_csv("D:/BigData/zaliczenie/dane/python_data.csv")
data_model_out.endog =pd.read_csv("D:/BigData/zaliczenie/dane/python_data_out.csv")
logit_mod=sm.Logit(data_model.exog, data_model_out.endog)