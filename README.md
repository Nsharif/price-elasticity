# price-elasticity
This repository is about calculating price elasticity models. 

I will be using the Berry Inversion method (logit) to calculate price elasticity. The value in using the Berry Inversion method in calculating price elasticity is that it essentially constructs the shape of the dependent variable in which the boundaries have an s shape rather than a linear or log shape.

Additionally, I will be using an instrument variable (IV) to address the endogeneity of price. 

I am using an IV because typically combinations of quantity and price that is observed in observational data reflects the forces of both demand and supply. Therefore, in general, the relationship I estimate is neither a demand curve nor a supply curve, but usually a complex mix of shifting demand and supply curves.

The general construction of an IV is via 2SLS (two stage least squares):
1st stage; endogenous variable is regressed on the IV or IV's, along with any other exogenous variables (controls)

2nd stage; the DV (dependent variable, quantity) is regressed on the predicted values (yhat) of the endogenous variable from the first stage, along with exogenous control variables. 

If the IV is valid, the estaimted coefficient of the predicted exogenous variable, in the 2nd stage, is an unbiased estimate of the desired parameter. 
