
par(mfrow=c(1,4))

load('~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/Four_model_compare_Module1/myOut.no.bTO_rep_3.856e+04_combo_01_params_P.speciation_0.2116_0.5036_0.2116_0.3713_P.extinction_0.102_0.3255_0.102_0.06732_P.diffusion_00_00_00_00_P.TO_00_00_00_00_P.Arisal_0.2004_0.03042_0.004665_0.2538_timesteps_30000_.Rdata')

plot(myOut$mytree, type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.col="cornflowerblue")

load('~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/Four_model_compare_Module1/myOut.no.bTO_rep_3.835e+04_combo_02_params_P.speciation_0.5564_0.4524_0.5564_0.7863_P.extinction_0.1911_0.3383_0.1911_0.529_P.diffusion_00_0.1473_0.6395_00_P.TO_00_00_00_00_P.Arisal_0.01108_0.05098_0.3731_0.1366_timesteps_30000_.Rdata')
plot(myOut$mytree, type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.col="cornflowerblue")

load('~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/Four_model_compare_Module1/myOut.no.bTO_rep_3.832e+04_combo_03_params_P.speciation_0.9558_0.1599_0.9558_0.1464_P.extinction_0.1174_0.4135_0.1174_0.002125_P.diffusion_00_00_00_00_P.TO_0.2332_0.3368_0.317_0.2208_P.Arisal_0.07039_0.08155_0.1593_0.1607_timesteps_30000_.Rdata')
plot(myOut$mytree, type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.col="cornflowerblue")



load('~/Box Sync/colliding ranges/Simulations_humans/big world cluster outputs/Four_model_compare_Module1/myOut.no.bTO_rep_3.804e+04_combo_04_params_P.speciation_0.5256_0.2206_0.5256_0.2204_P.extinction_0.3807_0.9196_0.3807_0.05152_P.diffusion_00_0.7301_0.9275_00_P.TO_0.2004_0.3951_0.1289_0.1986_P.Arisal_0.2965_0.03804_0.101_0.1984_timesteps_30000_.Rdata')
plot(myOut$mytree, type="fan", no.margin = TRUE, show.tip.label = FALSE, edge.col="cornflowerblue")