*Titulo: 				Cálculo IPS
*Autor: 				Axel Eduardo González Gómez (MCV)
*Fuentes:				IPS_bd
*Fecha de finalización: 



****************************Componente 1****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0101 ind_0102 ind_0103 ind_0104
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 2****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0101 ind_0102 ind_0103 ind_0104
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 2****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0105 ind_0106 ind_0107
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 3****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0108 ind_0109 ind_0110 ind_0111
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 4****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0112 ind_0113 ind_0114 ind_0115 ind_0116
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 5****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0117 ind_0118 ind_0119 ind_0120 ind_0121
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 6****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0122 ind_0123 ind_0124 ind_0125 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 7****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0126 ind_0127 ind_0128 ind_0129 ind_0130 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 8****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0131 ind_0132 ind_0133 ind_0134 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 9****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0135 ind_0136 ind_0137 ind_0138 ind_0139 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 10****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0140 ind_0141 ind_0142 ind_0143 ind_0144 
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo

****************************Componente 11****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0145 ind_0146 ind_0147 ind_0148 ind_0149  
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


****************************Componente 12****************************************

clear all
set more off

gl ruta=       	"C:/Users/axelo/Documents/GitHub/ips/02_bases_procesadas"

use "$ruta/01_ips_complete_wide.dta", clear 

global xlist ind_0150 ind_0151 ind_0152 ind_0153 ind_0154 ind_0155
global id State
global ncomp 3

describe $xlist
summarize $xlist
corr $xlist


* Average interitem covariance
alpha $xlist


* Factor analysis
factor $xlist

* Scree plot of the eigenvalues
screeplot
screeplot, yline(1)

predict f1

* KMO measure of sampling adequacy
estat kmo


