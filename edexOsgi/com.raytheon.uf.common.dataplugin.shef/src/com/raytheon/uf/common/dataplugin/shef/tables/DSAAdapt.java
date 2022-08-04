package com.raytheon.uf.common.dataplugin.shef.tables;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * Class to define dsaadapt database table.
 * <p>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * April 2018   DCS 20586   P Tilles    Rename column "max_usability_blk" to 
 *                                        "kdp_coeff_rain_hail"
 * 
 * 
 * </pre>
 * 
 * 
 */

@Entity
@Table(name = "dsaadapt")
@javax.xml.bind.annotation.XmlRootElement
@javax.xml.bind.annotation.XmlAccessorType(javax.xml.bind.annotation.XmlAccessType.NONE)
@com.raytheon.uf.common.serialization.annotations.DynamicSerialize
public class DSAAdapt
        extends com.raytheon.uf.common.dataplugin.persist.PersistableDataObject
        implements java.io.Serializable,
        com.raytheon.uf.common.serialization.ISerializableObject {

    private static final long serialVersionUID = 1L;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private DSAAdaptId id;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Short num_of_adap;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float default_ml_depth;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private String melt_layer_src;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float kdp_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float kdp_power;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float z_r_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float z_r_power;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float zdr_z_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float zdr_z_power;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float zdr_zdr_power;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float min_corr_precip;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float min_corr_kdp;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float refl_max;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float kdp_max_beam_blk;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float kdp_coeff_rain_hail;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float kdp_min_usage_rate;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float ws_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float gr_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float rh_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float ds_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float ic_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float grid_is_full;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float paif_rate;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float paif_area;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float rain_time_thresh;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float num_zones;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float max_precip_rate;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float restart_time;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float max_interp_time;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float max_hourly_acc;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float time_bias;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float num_grpairs;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float reset_bias;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float longst_lag;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float min_early_term_angle;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float max_volume_per_hour;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float dry_snow_mult;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float rkdp_use_thresh;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private String bias_applied_flag;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private String met_sig_proc_flag;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float met_sig_thresh;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private String cappi_proc_flag;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float cappi_thresh;

    @javax.xml.bind.annotation.XmlElement
    @com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement
    private Float cappi_height;

    public DSAAdapt() {
    }

    public DSAAdapt(DSAAdaptId id) {
        this.id = id;
    }

    public DSAAdapt(DSAAdaptId id, short num_of_adap, float default_ml_depth,
            String melt_layer_src, float kdp_mult, float kdp_power,
            float z_r_mult, float z_r_power, float zdr_z_mult,
            float zdr_z_power, float zdr_zdr_power, float min_corr_precip,
            float min_corr_kdp, float refl_max, float kdp_max_beam_blk,
            float kdp_coeff_rain_hail, float kdp_min_usage_rate, float ws_mult,
            float gr_mult, float rh_mult, float ds_mult, float ic_mult,
            float grid_is_full, float paif_rate, float paif_area,
            float rain_time_thresh, float num_zones, float max_precip_rate,
            float restart_time, float max_interp_time, float max_hourly_acc,
            float time_bias, float num_grpairs, float reset_bias,
            float longst_lag, float min_early_term_angle,
            float max_volume_per_hour, float dry_snow_mult,
            float rkdp_use_thresh, String bias_applied_flag,
            String met_sig_proc_flag, float met_sig_thresh,
            String cappi_proc_flag, float cappi_thresh, float cappi_height)

    {
        this.id = id;
        this.num_of_adap = num_of_adap;
        this.default_ml_depth = default_ml_depth;
        this.melt_layer_src = melt_layer_src;
        this.kdp_mult = kdp_mult;
        this.kdp_power = kdp_power;
        this.z_r_mult = z_r_mult;
        this.z_r_power = z_r_power;
        this.zdr_z_mult = zdr_z_mult;
        this.zdr_z_power = zdr_z_power;
        this.zdr_zdr_power = zdr_zdr_power;
        this.min_corr_precip = min_corr_precip;
        this.min_corr_kdp = min_corr_kdp;
        this.refl_max = refl_max;
        this.kdp_max_beam_blk = kdp_max_beam_blk;
        this.kdp_coeff_rain_hail = kdp_coeff_rain_hail; // new for Bld 19
        this.kdp_min_usage_rate = kdp_min_usage_rate;
        this.ws_mult = ws_mult;
        this.gr_mult = gr_mult;
        this.rh_mult = rh_mult;
        this.ds_mult = ds_mult;
        this.ic_mult = ic_mult;
        this.grid_is_full = grid_is_full;
        this.paif_rate = paif_rate;
        this.paif_area = paif_area;
        this.rain_time_thresh = rain_time_thresh;
        this.num_zones = num_zones;
        this.max_precip_rate = max_precip_rate;
        this.restart_time = restart_time;
        this.max_interp_time = max_interp_time;
        this.max_hourly_acc = max_hourly_acc;
        this.time_bias = time_bias;
        this.num_grpairs = num_grpairs;
        this.reset_bias = reset_bias;
        this.longst_lag = longst_lag;
        this.min_early_term_angle = min_early_term_angle;
        this.max_volume_per_hour = max_volume_per_hour;
        this.dry_snow_mult = dry_snow_mult;
        this.rkdp_use_thresh = rkdp_use_thresh;
        this.bias_applied_flag = bias_applied_flag;
        this.met_sig_proc_flag = met_sig_proc_flag;
        this.met_sig_thresh = met_sig_thresh;
        this.cappi_proc_flag = cappi_proc_flag;
        this.cappi_thresh = cappi_thresh;
        this.cappi_height = cappi_height;
    }

    @EmbeddedId
    @AttributeOverrides({
            @AttributeOverride(name = "radid", column = @Column(name = "radid", nullable = false, length = 3) ),
            @AttributeOverride(name = "obstime", column = @Column(name = "obstime", nullable = false, length = 29) ) })
    public DSAAdaptId getId() {
        return this.id;
    }

    public void setId(DSAAdaptId id) {
        this.id = id;
    }

    @Column(name = "num_of_adap")
    public Short getNumOfAdap() {
        return this.num_of_adap;
    }

    public void setNumOfAdap(Short num_of_adap) {
        this.num_of_adap = num_of_adap;
    }

    @Column(name = "default_ml_depth")
    public Float getDefaultMlDepth() {
        return this.default_ml_depth;
    }

    public void setDefaultMlDepth(Float default_ml_depth) {
        this.default_ml_depth = default_ml_depth;
    }

    @Column(name = "melt_layer_src")
    public String getMeltLayerSrc() {
        return this.melt_layer_src;
    }

    public void setMeltLayerSrc(String melt_layer_src) {
        this.melt_layer_src = melt_layer_src;
    }

    @Column(name = "kdp_mult")
    public Float getKdpMult() {
        return this.kdp_mult;
    }

    public void setKdpMult(Float kdp_mult) {
        this.kdp_mult = kdp_mult;
    }

    @Column(name = "kdp_power")
    public Float getKdpPower() {
        return this.kdp_power;
    }

    public void setKdpPower(Float kdp_power) {
        this.kdp_power = kdp_power;
    }

    @Column(name = "z_r_mult")
    public Float getZRMult() {
        return this.z_r_mult;
    }

    public void setZRMult(Float z_r_mult) {
        this.z_r_mult = z_r_mult;
    }

    @Column(name = "z_r_power")
    public Float getZrPower() {
        return this.z_r_power;
    }

    public void setZrPower(Float z_r_power) {
        this.z_r_power = z_r_power;
    }

    @Column(name = "zdr_z_mult")
    public Float getZdrZMult() {
        return this.zdr_z_mult;
    }

    public void setZdrZMult(Float zdr_z_mult) {
        this.zdr_z_mult = zdr_z_mult;
    }

    @Column(name = "zdr_z_power")
    public Float getZdrZPower() {
        return this.zdr_z_power;
    }

    public void setZdrZPower(Float zdr_z_power) {
        this.zdr_z_power = zdr_z_power;
    }

    @Column(name = "zdr_zdr_power")
    public Float getZdrZdrPower() {
        return this.zdr_zdr_power;
    }

    public void setZdrZdrPower(Float zdr_zdr_power) {
        this.zdr_zdr_power = zdr_zdr_power;
    }

    @Column(name = "min_corr_precip")
    public Float getMinCorrPrecip() {
        return this.min_corr_precip;
    }

    public void setMinCorrPrecip(Float min_corr_precip) {
        this.min_corr_precip = min_corr_precip;
    }

    @Column(name = "min_corr_kdp")
    public Float getMinCorrKdp() {
        return this.min_corr_kdp;
    }

    public void setMinCorrKdp(Float min_corr_kdp) {
        this.min_corr_kdp = min_corr_kdp;
    }

    @Column(name = "refl_max")
    public Float getReflMax() {
        return this.refl_max;
    }

    public void setReflMax(Float refl_max) {
        this.refl_max = refl_max;
    }

    @Column(name = "kdp_max_beam_blk")
    public Float getKdpMaxBeamBlk() {
        return this.kdp_max_beam_blk;
    }

    public void setKdpMaxBeamBlk(Float kdp_max_beam_blk) {
        this.kdp_max_beam_blk = kdp_max_beam_blk;
    }

    @Column(name = "kdp_coeff_rain_hail")
    public Float getKDPCoeffRainHail() {
        return this.kdp_coeff_rain_hail;
    }

    public void setKDPCoeffRainHail(Float kdp_coeff_rain_hail) {
        this.kdp_coeff_rain_hail = kdp_coeff_rain_hail;
    }

    @Column(name = "kdp_min_usage_rate")
    public Float getKdpMinUsageRate() {
        return this.kdp_min_usage_rate;
    }

    public void setKdpMinUsageRate(Float kdp_min_usage_rate) {
        this.kdp_min_usage_rate = kdp_min_usage_rate;
    }

    @Column(name = "ws_mult")
    public Float getWsMult() {
        return this.ws_mult;
    }

    public void setWsMult(Float ws_mult) {
        this.ws_mult = ws_mult;
    }

    @Column(name = "gr_mult")
    public Float getGrMult() {
        return this.gr_mult;
    }

    public void setGrMult(Float gr_mult) {
        this.gr_mult = gr_mult;
    }

    @Column(name = "rh_mult")
    public Float getRhMult() {
        return this.rh_mult;
    }

    public void setRhMult(Float rh_mult) {
        this.rh_mult = rh_mult;
    }

    @Column(name = "ds_mult")
    public Float getDsMult() {
        return this.ds_mult;
    }

    public void setDsMult(Float ds_mult) {
        this.ds_mult = ds_mult;
    }

    @Column(name = "ic_mult")
    public Float getIcMult() {
        return this.ic_mult;
    }

    public void setIcMult(Float ic_mult) {
        this.ic_mult = ic_mult;
    }

    @Column(name = "grid_is_full")
    public Float getGridIsFull() {
        return this.grid_is_full;
    }

    public void setGridIsFull(Float grid_is_full) {
        this.grid_is_full = grid_is_full;
    }

    @Column(name = "paif_rate")
    public Float getPaifRate() {
        return this.paif_rate;
    }

    public void setPaifRate(Float paif_rate) {
        this.paif_rate = paif_rate;
    }

    @Column(name = "paif_area")
    public Float getPaifArea() {
        return this.paif_area;
    }

    public void setPaifArea(Float paif_area) {
        this.paif_area = paif_area;
    }

    @Column(name = "rain_time_thresh")
    public Float getRainTimeThresh() {
        return this.rain_time_thresh;
    }

    public void setRainTimeThresh(Float rain_time_thresh) {
        this.rain_time_thresh = rain_time_thresh;
    }

    @Column(name = "num_zones")
    public Float getNumZones() {
        return this.num_zones;
    }

    public void setNumZones(Float num_zones) {
        this.num_zones = num_zones;
    }

    @Column(name = "max_precip_rate")
    public Float getMaxPrecipRate() {
        return this.max_precip_rate;
    }

    public void setMaxPrecipRate(Float max_precip_rate) {
        this.max_precip_rate = max_precip_rate;
    }

    @Column(name = "restart_time")
    public Float getRestartTime() {
        return this.restart_time;
    }

    public void setRestartTime(Float restart_time) {
        this.restart_time = restart_time;
    }

    @Column(name = "max_interp_time")
    public Float getMaxInterpTime() {
        return this.max_interp_time;
    }

    public void setMaxInterpTime(Float max_interp_time) {
        this.max_interp_time = max_interp_time;
    }

    @Column(name = "max_hourly_acc")
    public Float getMaxHourlyAcc() {
        return this.max_hourly_acc;
    }

    public void setMaxHourlyAcc(Float max_hourly_acc) {
        this.max_hourly_acc = max_hourly_acc;
    }

    @Column(name = "time_bias")
    public Float getTimeBias() {
        return this.time_bias;
    }

    public void setTimeBias(Float time_bias) {
        this.time_bias = time_bias;
    }

    @Column(name = "num_grpairs")
    public Float getNumGrPairs() {
        return this.num_grpairs;
    }

    public void setNumGrPairs(Float num_gr_pairs) {
        this.num_grpairs = num_gr_pairs;
    }

    @Column(name = "reset_bias")
    public Float getResetBias() {
        return this.reset_bias;
    }

    public void setResetBias(Float reset_bias) {
        this.reset_bias = reset_bias;
    }

    @Column(name = "longst_lag")
    public Float getLongstLag() {
        return this.longst_lag;
    }

    public void setLongstLag(Float longst_lag) {
        this.longst_lag = longst_lag;
    }

    @Column(name = "min_early_term_angle")
    public Float getMinEarlyTermAngle() {
        return this.min_early_term_angle;
    }

    public void setMinEarlyTermAngle(Float min_early_term_angle) {
        this.min_early_term_angle = min_early_term_angle;
    }

    @Column(name = "max_volume_per_hour")
    public Float getMaxVolumePerHour() {
        return this.max_volume_per_hour;
    }

    public void setMaxVolumePerHour(Float max_volume_per_hour) {
        this.max_volume_per_hour = max_volume_per_hour;
    }

    @Column(name = "dry_snow_mult")
    public Float getDrySnowMult() {
        return this.dry_snow_mult;
    }

    public void setDrySnowMult(Float dry_snow_mult) {
        this.dry_snow_mult = dry_snow_mult;
    }

    @Column(name = "rkdp_use_thresh")
    public Float getRkdpUseThresh() {
        return this.rkdp_use_thresh;
    }

    public void setRkdpUseThresh(Float rkdp_use_thresh) {
        this.rkdp_use_thresh = rkdp_use_thresh;
    }

    @Column(name = "bias_applied_flag")
    public String getBiasAppliedFlag() {
        return this.bias_applied_flag;
    }

    public void setBiasAppliedFlag(String bias_applied_flag) {
        this.bias_applied_flag = bias_applied_flag;
    }

    @Column(name = "met_sig_proc_flag")
    public String getMetSigProcFlag() {
        return this.met_sig_proc_flag;
    }

    public void setMetSigProcFlag(String met_sig_proc_flag) {
        this.met_sig_proc_flag = met_sig_proc_flag;
    }

    @Column(name = "met_sig_thresh")
    public Float getMetSigThresh() {
        return this.met_sig_thresh;
    }

    public void setMetSigThresh(Float met_sig_thresh) {
        this.met_sig_thresh = met_sig_thresh;
    }

    @Column(name = "cappi_proc_flag")
    public String getCappiProcFlag() {
        return this.cappi_proc_flag;
    }

    public void setCappiProcFlag(String cappi_proc_flag) {
        this.cappi_proc_flag = cappi_proc_flag;
    }

    @Column(name = "cappi_thresh")
    public Float getCappiThresh() {
        return this.cappi_thresh;
    }

    public void setCappiThresh(Float cappi_thresh) {
        this.cappi_thresh = cappi_thresh;
    }

    @Column(name = "cappi_height")
    public Float getCappiHeight() {
        return this.cappi_height;
    }

    public void setCappiHeight(Float cappi_height) {
        this.cappi_height = cappi_height;
    }
}
