*Titulo: 				Cálculo IPS
*Autor: 				Axel Eduardo González Gómez (MCV)
*Fuentes:				IPS_bd
*Fecha de finalización: 





clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

gl log="C:/Users/axelo/Documents/GitHub/ips"

log using "$log\analisis factorial.smcl", replace

****************************Componente 1****************************************

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0101 ind_0102 ind_0103 ind_0104
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo



****************************Componente 2****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0105 ind_0106 ind_0107
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 3****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0108 ind_0109 ind_0110 ind_0111
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 4****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0112 ind_0113 ind_0114 ind_0115 ind_0116
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 5****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0217 ind_0218 ind_0219 ind_0220 ind_0221
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 6****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0222 ind_0223 ind_0224 ind_0225 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 7****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0226 ind_0227 ind_0228 ind_0229 ind_0230 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 8****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0231 ind_0232 ind_0233 ind_0234 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 9****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0335 ind_0336 ind_0337 ind_0338 ind_0339 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 10****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0340 ind_0341 ind_0342 ind_0343 ind_0344 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 11****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0345 ind_0346 ind_0347 ind_0348 ind_0349  
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 12****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/03_ips_clean"

import excel "$ruta/00_ips_wide_axel.xlsx", sheet("Sheet 1") firstrow

global xlist ind_0350 ind_0351 ind_0352 ind_0353 ind_0354 ind_0355
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist, pcf

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


