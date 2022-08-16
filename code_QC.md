library(ggpubr)
QC_function_try <- function(seurat){
# Visualize the number UMIs/transcripts per cell
p1=seurat@meta.data %>%
ggplot(aes(color=orig.ident, x=nCount_RNA, fill= orig.ident)) + 
  	geom_density(alpha = 0.2) + 
  	scale_x_log10() + 
  	theme_classic() +
  	ylab("Cell density") +
  	geom_vline(xintercept = 500)
# Visualize the distribution of genes detected per cell via histogram
p2=seurat@meta.data %>%
  	ggplot(aes(color=orig.ident, x=nFeature_RNA, fill= orig.ident)) + 
  	geom_density(alpha = 0.2) + 
  	theme_classic() +
  	scale_x_log10() + 
  	geom_vline(xintercept = 300)
p3=seurat@meta.data %>%
  	ggplot(aes(x=nCount_RNA, y=nFeature_RNA, color=percent.mt)) + 
  	geom_point() + 
	scale_colour_gradient(low = "gray90", high = "black") +
  	stat_smooth(method=lm) +
  	scale_x_log10() + 
  	scale_y_log10() + 
  	theme_classic() +
  	geom_vline(xintercept = 500) +
  	geom_hline(yintercept = 300)
p4=seurat@meta.data %>% 
  	ggplot(aes(color=orig.ident, x=percent.mt, fill=orig.ident)) + 
  	geom_density(alpha = 0.2) + 
  	scale_x_log10() + 
  	theme_classic() +
  	geom_vline(xintercept = 20)
p5=seurat@meta.data %>%
ggplot(aes(x=nFeature_RNA, y=percent.mt)) + 
  	geom_point() + 
  	stat_smooth(method=lm) +
  scale_x_log10() + 
  	scale_y_log10() + 
  	theme_classic()
p6=seurat@meta.data %>%
ggplot(aes(x=log10GenesPerUMI, color = orig.ident, fill=orig.ident)) +
  	geom_density(alpha = 0.2) +
  	theme_classic() +
  	geom_vline(xintercept = 0.76)
p7=FeatureScatter(seurat, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
p8=FeatureScatter(seurat, feature1 = "nFeature_RNA", feature2 = "percent.mt")
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=3,nrow=3)
}
