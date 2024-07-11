library(fsbrain)
library(data.table)
library(stringr)
library(dplyr)
library(gridExtra)
library(grid)
library(png)

#read in summary statistic files:
sFiles <- list.files("ENIGMA/enigmatoolbox/datasets/summary_statistics/", pattern = "*.csv", full.names = TRUE)
summaryStats <- lapply(sFiles, fread)
names(summaryStats) <- basename(sFiles)

#define plot aggregates:
mapTypesRegex <- c("CorticalThickness", "SurfaceArea", "SubcorticalVolume|SuborticalVolume")

typeVec <- rep(NA, length(summaryStats))
for(type in mapTypesRegex){
  for(i in seq_along(summaryStats)){
    if (any(str_detect(names(summaryStats[[i]]), type))){
      typeVec[[i]] <- type
    }
  }
}

# make one giga map per map Type:
CT <- summaryStats[typeVec == "CorticalThickness"]
SA <- summaryStats[typeVec == "SurfaceArea"]
SCV <- summaryStats[typeVec == "SubcorticalVolume|SuborticalVolume"]

#map Unspecific CT
subject_id <- 'fsaverage'
atlas <- 'aparc'
individualImagePath <- file.path("MAPS", "IndividualMaps", "CorticalThickness")
subjectDir <- "/Applications/freesurfer/7.3.2/subjects"
atlas_region_names_left  <-  data.frame("atlasROI" = paste0("L_", get.atlas.region.names(atlas, template_subjects_dir=subjectDir, template_subject=subject_id)),
                                        "origROI" = get.atlas.region.names(atlas, template_subjects_dir=subjectDir, template_subject=subject_id))
atlas_region_names_right  <-  data.frame("atlasROI" = paste0("R_", get.atlas.region.names(atlas, template_subjects_dir=subjectDir, template_subject=subject_id)),
                                         "origROI" = get.atlas.region.names(atlas, template_subjects_dir=subjectDir, template_subject=subject_id))

dir.create(individualImagePath, recursive = T, showWarnings = FALSE)
#map specific CT
pb <- txtProgressBar(min = 0, max = length(CT), style = 3, width = length(CT), char = "=") 
for(i in seq_along(CT)){
  setTxtProgressBar(pb, i)
  lhValDF <- merge(x = atlas_region_names_left, y = CT[[i]], by.x = "atlasROI", by.y = "Structure", all.x = TRUE)
  rhValDF <- merge(x = atlas_region_names_right, y = CT[[i]], by.x = "atlasROI", by.y = "Structure", all.x = TRUE)
  lhValDF$d_icv[lhValDF$origROI == "unknown"] <- .99
  rhValDF$d_icv[rhValDF$origROI == "unknown"] <- -.99
  lhVal <- lhValDF$d_icv
  names(lhVal) <- lhValDF$origROI
  lhVal[lhValDF$fdr_p > 0.05] <- NA
  rhVal <- rhValDF$d_icv
  names(rhVal) <- rhValDF$origROI
  rhVal[rhValDF$fdr_p > 0.05] <- NA
  makecmap_options <-  list("colFn" = colorRampPalette(c("darkblue", "white", "darkred")), 'symm'=TRUE, breaks = seq(-1, 1 , length.out = 201)) #'colFn'=colorRampPalette(c("Red", "Blue")),
  rgloptions <- list("windowRect"=c(50,50,1000,1000))
  cm <- vis.region.values.on.subject(subjects_dir = subjectDir,
                                     subject_id = subject_id,
                                     atlas = atlas,
                                     lh_region_value_list = lhVal,
                                     rh_region_value_list = rhVal,
                                     views=c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
                                     draw_colorbar = TRUE,
                                     makecmap_options = makecmap_options,
                                     rgloptions = rgloptions,
                                     # rglactions=list('no_vis'=T),
                                     style = "default")
  indivPlots <- fsbrain::export(cm,
                                view_angles = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
                                grid_like = FALSE,
                                draw_colorbar = "horizontal",
                                colorbar_legend = "Cohen's d",
                                large_legend = TRUE,
                                output_img = file.path(individualImagePath, paste0(tools::file_path_sans_ext(CT[[i]]$CorticalThickness[1]), ".png")))
                                # output_img = "test.png")
  rgl::close3d(rgl::rgl.dev.list())
}

#map Unspecific SA
subject_id <- 'fsaverage'
atlas <- 'aparc'
individualImagePath <- file.path("MAPS", "IndividualMaps", "SurfaceArea")
subjectDir <- "/Applications/freesurfer/7.3.2/subjects"
atlas_region_names_left  <-  data.frame("atlasROI" = paste0("L_", get.atlas.region.names(atlas, template_subjects_dir=subjectDir, template_subject=subject_id)),
                                        "origROI" = get.atlas.region.names(atlas, template_subjects_dir=subjectDir, template_subject=subject_id))
atlas_region_names_right  <-  data.frame("atlasROI" = paste0("R_", get.atlas.region.names(atlas, template_subjects_dir=subjectDir, template_subject=subject_id)),
                                         "origROI" = get.atlas.region.names(atlas, template_subjects_dir=subjectDir, template_subject=subject_id))

dir.create(individualImagePath, recursive = T, showWarnings = FALSE)
#map specific SA
pb <- txtProgressBar(min = 0, max = length(SA), style = 3, width = length(SA), char = "=") 
for(i in seq_along(SA)){
  setTxtProgressBar(pb, i)
  lhValDF <- merge(x = atlas_region_names_left, y = SA[[i]], by.x = "atlasROI", by.y = "Structure", all.x = TRUE)
  rhValDF <- merge(x = atlas_region_names_right, y = SA[[i]], by.x = "atlasROI", by.y = "Structure", all.x = TRUE)
  lhValDF$d_icv[lhValDF$origROI == "unknown"] <- .99
  rhValDF$d_icv[rhValDF$origROI == "unknown"] <- -.99
  lhVal <- lhValDF$d_icv
  names(lhVal) <- lhValDF$origROI
  lhVal[lhValDF$fdr_p > 0.05] <- NA
  rhVal <- rhValDF$d_icv
  names(rhVal) <- rhValDF$origROI
  rhVal[rhValDF$fdr_p > 0.05] <- NA
  rhVal <- pmax(pmin(rhVal, 1), -1)
  lhVal <- pmax(pmin(lhVal, 1), -1)
  makecmap_options <-  list("colFn" = colorRampPalette(c("darkblue", "white", "darkred")), 'symm'=TRUE, breaks = seq(-1, 1 , length.out = 201)) #'colFn'=colorRampPalette(c("Red", "Blue")),
  rgloptions <- list("windowRect"=c(50,50,1000,1000))
  cm <- vis.region.values.on.subject(subjects_dir = subjectDir,
                                     subject_id = subject_id,
                                     atlas = atlas,
                                     lh_region_value_list = lhVal,
                                     rh_region_value_list = rhVal,
                                     views=c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
                                     draw_colorbar = TRUE,
                                     makecmap_options = makecmap_options,
                                     rgloptions = rgloptions,
                                     rglactions=list('no_vis'=T),
                                     style = "default")
  indivPlots <- fsbrain::export(cm,
                                view_angles = c("sd_lateral_lh", "sd_medial_lh", "sd_lateral_rh", "sd_medial_rh"),
                                grid_like = FALSE,
                                draw_colorbar = "horizontal",
                                large_legend = TRUE,
                                colorbar_legend = "Cohen's d",
                                output_img = file.path(individualImagePath, paste0(tools::file_path_sans_ext(SA[[i]]$SurfaceArea[1]), ".png")))
  rgl::close3d(rgl::rgl.dev.list())
}


#map Unspecific SCV
individualImagePath <- file.path("MAPS", "IndividualMaps", "SubcorticalVolume")
atlasRegions <- c("L-accumbens",	"L-amygdala",	"L-caudate",	"L-hippocampus",	"L-pallidum",	"L-putamen",	"L-thalamus",	"L-ventricle",	"R-accumbens",	"R-amygdala",	"R-caudate",	"R-hippocampus",	"R-pallidum",	"R-putamen",	"R-thalamus",	"R-ventricle")
translateSCVnames <- Vectorize(function(r){
  if (r == "L-accumbens") return("Laccumb")
  if (r == "L-amygdala") return("Lamyg")
  if (r == "L-caudate") return("Lcaud")
  if (r == "L-hippocampus") return("Lhippo")
  if (r == "L-pallidum") return("Lpal")
  if (r == "L-putamen") return("Lput")
  if (r == "L-thalamus") return("Lthal")
  if (r == "L-ventricle") return("LLatVent")
  if (r == "R-accumbens") return("Raccumb")
  if (r == "R-amygdala") return("Ramyg")
  if (r == "R-caudate") return("Rcaud")
  if (r == "R-hippocampus") return("Rhippo")
  if (r == "R-pallidum") return("Rpal")
  if (r == "R-putamen") return("Rput")
  if (r == "R-thalamus") return("Rthal")
  if (r == "R-ventricle") return("RLatVent")
  return(NA)
})
atlas_region_names  <-  data.frame("atlasROI" = translateSCVnames(atlasRegions),
                                        "origROI" = atlasRegions)


dir.create(individualImagePath, recursive = T, showWarnings = FALSE)
#map specific SCV
pb <- txtProgressBar(min = 0, max = length(SCV), style = 3, width = length(SCV), char = "=") 
for(i in seq_along(SCV)){
  setTxtProgressBar(pb, i)
  ValDF <- merge(x = atlas_region_names, y = SCV[[i]], by.x = "atlasROI", by.y = "Structure", all.x = TRUE)
  ValDF <- ValDF[match( atlasRegions, ValDF$origROI),]
  output_img = file.path(individualImagePath, paste0(tools::file_path_sans_ext(SCV[[i]][1,1]), ".png"))
  ventricles = "True"
  if(is.na(ValDF$d_icv[8]) & is.na(ValDF$d_icv[8])){
    ventricles = "False"
  } else {
    ValDF$d_icv[is.na(ValDF$d_icv)] <- 0
  }
  vals <- na.omit(pmax(pmin(ValDF$d_icv, 1), -1))
  matlabCmd <- paste0(
    "/Applications/MATLAB_R2023b.app/bin/matlab -nodesktop -nosplash -r \"plot_subcortical([",
    paste(vals, collapse = " "),
    "], 'ventricles', '", ventricles, "', 'color_range', [-1 1]); f = gcf; exportgraphics(f,'", 
    output_img,
    "','Resolution',300); exit\""
  )
  print(ValDF$d_icv)
  system(matlabCmd)
  #
}



# make summary plot:
comps <- list.files("MAPS/IndividualMaps", pattern = "*.png", full.names = TRUE, recursive = TRUE)
unique(basename(comps))

dfcomps <- data.frame(comps = tools::file_path_sans_ext(basename(comps)), type = str_remove(dirname(comps), "MAPS/IndividualMaps/"), path = comps)
dfCompsWide <-pivot_wider(dfcomps, names_from = type, values_from = path, values_fill = NA)

read_image <- function(path) {
  if (is.na(path)) {
    # Return a white image (empty space)
    matrix(1, nrow = 100, ncol = 100)
  } else {
    # Read the actual image (adjust dimensions as needed)
    png::readPNG(path, native = TRUE)
  }
}


# Define the number of rows and columns
num_rows <- nrow(dfCompsWide)
num_cols <- ncol(dfCompsWide)

# Populate the viewport with images
gl <- list()
for (i in 1:num_rows) {grid_table
  for (j in 2:num_cols) {
    img_path <- unlist(dfCompsWide[i, j])
    img <- read_image(img_path)
    img_grob <- rasterGrob(img)
    gl <- append(gl, list(img_grob))
  }
}

# Arrange the composite image and the grid table
final_grob <- rbind(tableGrob(t(names(dfCompsWide)[-1]), theme = ttheme_minimal(base_size = 72, padding = unit(c(0, 1/(num_rows+13)), "npc")), rows = ""), 
                    cbind(tableGrob(dfCompsWide$comps, theme = ttheme_minimal(base_size = 72)), 
                          arrangeGrob(grobs = gl, layout_matrix = matrix(seq(length(gl)), ncol = 3, byrow = TRUE), ncol = 3, widths = c(4, 4, 4), as.table = FALSE),
                          size = "last"), size = "last")

# Save the final image
png("composite_image.png", width = 6000, height = 25000)
grid.draw(final_grob)
grid.polygon(x = rep(c(0,0.5,1), num_rows),
             y = rep(seq(num_rows), each = 3) / (num_rows+1),  #0.97183099 correct last number (approx?)
             id = rep(seq(num_rows), each = 3),
             gp=gpar(lwd=6))
dev.off()
 





