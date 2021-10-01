library(rgdal)

ecoreg <- readOGR('data/tropcEco211_simpy0d005.shp', stringsAsFactors = FALSE)
ecoreg$ECO_NAME <- iconv(ecoreg$ECO_NAME, from = 'utf8', to = 'utf8')
#x <- iconv(ecoreg$ECO_NAME, from = 'utf8', to = 'latin1')
table(unique(ecoreg@data[, c('REALM', 'ECO_NAME')])$REALM)
dim(unique(subset(unique(ecoreg@data[, c('REALM', 'ECO_NAME')]), REALM == 'Neotropic')))

length(unique(ecoreg$ECO_NAME))
length(unique(ecoreg$REALM))
length(unique(ecoreg$BIOME_NAME))

dim(ecoreg)
# library(ggplot2)
# library(scales)
# n = 5
# image(
#   1:n, 1, as.matrix(1:n),
#   col = viridis(n, option = "D"),
#   xlab = "viridis n", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
# )

library(viridisLite)
ecoreg$ECO_ecoID <- as.numeric(as.factor(ecoreg$ECO_ID))
ecoreg$ECO_col <- viridis(length(unique(ecoreg$ECO_NAME)), option = "D")[ecoreg$ECO_ecoID]
ecoreg$REALM_NUM <- as.numeric(as.factor(ecoreg$REALM))
ecoreg$REALM_col <- viridis(length(unique(ecoreg$REALM)), option = "D")[ecoreg$REALM_NUM]
ecoreg$BIOME_col <- viridis(5, option = "D")[4]
unique(ecoreg@data[, c('REALM', 'REALM_NUM')])

save(ecoreg, file = 'data/ecoreg_shp.RData')


# load('data/compiled__trans_df_7596.RData') # trans
# load('data/compiled_MSPA_df_50files.RData') # df


