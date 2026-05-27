--Change name of column "ml_overide_flag" to "melt_layer_src"

ALTER TABLE DSAAdapt RENAME COLUMN ml_overide_flag TO melt_layer_src;

--Add new columns for Build 17 parameters

ALTER TABLE DSAAdapt ADD COLUMN min_early_term_angle   float4;
ALTER TABLE DSAAdapt ADD COLUMN max_volume_per_hour    float4;
ALTER TABLE DSAAdapt ADD COLUMN dry_snow_mult          float4;
ALTER TABLE DSAAdapt ADD COLUMN rkdp_use_thresh        float4;
ALTER TABLE DSAAdapt ADD COLUMN bias_applied_flag  varchar(3);
ALTER TABLE DSAAdapt ADD COLUMN met_sig_proc_flag  varchar(3);
ALTER TABLE DSAAdapt ADD COLUMN met_sig_thresh         float4;
ALTER TABLE DSAAdapt ADD COLUMN cappi_proc_flag    varchar(3);
ALTER TABLE DSAAdapt ADD COLUMN cappi_thresh           float4;
ALTER TABLE DSAAdapt ADD COLUMN cappi_height           float4;

