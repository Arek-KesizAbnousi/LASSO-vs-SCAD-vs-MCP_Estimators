library(ncvreg)
library(glmnet)

# to compare the three penalties.Perform 50 independent replications for different sigma and gamma.

penalty(0.5, gamma_MCP = 3, gamma_SCAD = 3.7)
penalty(0.5, gamma_MCP = 3, gamma_SCAD = 3)
penalty(0.5, gamma_MCP = 4, gamma_SCAD = 3.7)
penalty(1, gamma_MCP = 3, gamma_SCAD = 3.7)
penalty(1, gamma_MCP = 3, gamma_SCAD = 3)
penalty(1, gamma_MCP = 4, gamma_SCAD = 3.7)
penalty(2, gamma_MCP = 3, gamma_SCAD = 3.7)
penalty(2, gamma_MCP = 3, gamma_SCAD = 3)
penalty(2, gamma_MCP = 4, gamma_SCAD = 3.7)
penalty(3, gamma_MCP = 3, gamma_SCAD = 3.7)
penalty(3, gamma_MCP = 3, gamma_SCAD = 3)
penalty(3, gamma_MCP = 4, gamma_SCAD = 3.7)