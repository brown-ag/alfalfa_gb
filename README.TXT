ROSETTA HELP FILE (August, 1999)

==========================================
PREDICTING SOIL HYDRAULIC 
PARAMETERS FROM BASIC SOIL DATA
==========================================
Rosetta Version 1.0

Marcel Schaap
U.S. Salinity Laboratory
450 W. Big Springs Road
Riverside, CA 92507
USA
mschaap@ussl.ars.usda.gov
==========================================

I. BACKGROUND
---------------------------

Soil hydraulic characteristics (water retention and
hydraulic conductivity) are required for many studies of
water and solute transport in the vadose zone. Although
measurements are the most obvious and precise way to obtain
these characteristics, financial and time constraints place
limits on how much can be determined in the field or
laboratory. Spatial variability of soil hydraulic
characteristics further makes it doubtful whether limited
soil hydraulic measurements are representative for the
studied area. Alternatively, soil hydraulic characteristics
can be estimated using pedotransfer functions (PTFs) that
use widely available basic soil data (texture, bulk
density, etc) as predictors. Although predictions by PTFs
are not as accurate as measurements, they provide a way to
obtain soil hydraulic characteristics that would otherwise
not have been available. 

Rosetta implements PTFs to predict van Genuchten (1980)
water retention parameters  and saturated hydraulic
conductivity (Ks) by using textural class, textural
distribution, bulk density and one or two water retention
points as input. Although the use of more input data often
leads to better predictions (Schaap and Bouten, 1996;
Schaap et al., 1998), there are many cases where only
limited soil information is available. Rosetta follows a
hierarchical approach to estimate water retention and Ks
values using limited or more extended sets of input data
(Schaap et al., 1998, Schaap and Leij, 1998a). The
hierarchical approach is reflected in the five models. The
simplest model (TXT) consists of a lookup table for average
hydraulic parameters for each soil textural class  (sand,
silty loam, clay loam, etc.). The other four models are
based on neural network analysis (Schaap et al., 1998) and
predict the hydraulic parameters, using additional input
variables, with an increasing degree of accuracy. All five
models have been calibrated on the same data set and
provide consistent predictions. The calibration data set
contained 2134 samples for water retention, 1306 samples
for Ks (Schaap and Leij, 1998b) and 235 for unsaturated
hydraulic conductivity (Schaap and Leij, 1999). The samples
were obtained from a large number of sources and involve
agricultural and non-agricultural soils in temperate
climate zones of the northern hemisphere (mainly from the
USA and some from Europe). Usage of Rosetta for other
climate zones, and hence other pedogenic processes, could
lead to inaccurate predictions. The reader is referred to
Schaap et al. (1998) and Schaap and Leij (1998a,b) for more
information about the calibration of the PTFs in Rosetta.


II. DISCLAIMER AND IMPORTANT INFORMATION
-------------------------------------------------------------------------------

Although the data for the calibration of the pedotransfer
functions were carefully selected, we do not assume any
responsibility for the correctness and usability of the
predicted hydraulic properties. 

It is very important to note that Rosetta and its
predictions are subject to change! Rosetta is a result of a
continuing research project and future versions will
probably include additional PTFs that predict different
hydraulic functions or use other sets of input data. Due to
the calibration on additional data, the current PTFs may
become more applicable to a wider range of soils but may
also yield slightly different estimates of the hydraulic
parameters.  

Please check at the USSL web site
(http::\\www.ussl.ars.usda.gov) for the latest information
about Rosetta. At this site a more extended version of
Rosetta can be found to predict hydraulic parameters from
large amounts of input data. This version will also
provide uncertainty estimates of the predicted hydraulic
parameters which can be used to assess the reliability of
the predictions.


III. REFERENCES
----------------------------

Schaap, M.G. and W. Bouten. 1996. Modeling water retention
curves of sandy soils using neural networks. Water Resour.
Res. 32:3033-3040.

Schaap, M.G., Leij F.J. and van Genuchten M.Th. 1998.
Neural network analysis for hierarchical prediction of soil
water retention and saturated hydraulic conductivity. Soil
Sci. Soc. Am. J.  62:847-855.

Schaap, M.G. and F.J. Leij. 1998a. Using Neural Networks to
predict soil water retention and soil hydraulic
conductivity. Soil & Tillage Research 47:37-42.

Schaap, M.G., and F.J. Leij, 1998b. Database Related
Accuracy and Uncertainty of Pedotransfer Functions. Soil
Science 163:765-779.

Schaap, M.G., and F.J. Leij, 1999. Improved prediction of
unsaturated hydraulic conductivity with the Mualem-van
Genuchten equation. Submitted to Soil Sci. Soc Am. J.

van Genuchten, M.Th. 1980. A closed-form equation for
predicting the hydraulic conductivity of unsaturated soils.
Soil Sci. Am. J. 44:892-898.
 
