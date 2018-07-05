package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import java.util.ArrayList;
import java.util.List;

/***
 * 
 * This enumeration contains a list of all model family definitions and is
 * analogous to the Model Family menu options in the CAVE Volume menu.
 * 
 * To understand more about model families, raw model families can be found in
 * the <code>com.raytheon.viz.volumebrowser</code> under the directory
 * <code>localization/bundles/volume</code>. There you will find top-level
 * families like <code>WinterFamily.xml</code> and its related inner family
 * <code>ModelFamilyD.xml</code>.
 * 
 * TODO: Storing configuration information into enumerations is not acceptable
 * for the longer term. Since the model family raw files are stable for now, we
 * will plan to change this in the next release.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 20, 2016   12371     polster     Initial creation
 * Dec 01, 2017   41520     polster     Class name ModelSources changed to ModelSourceKind
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public enum ModelFamilyDefinitions {

    /* Hail Family */

    HAIL_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyC.xml",
            "TP",
            41), //

    HAIL_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyC.xml",
            "TP",
            55), //

    HAIL_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.LAPS,
            "bundles/volume/ModelFamilyC.xml",
            "TP",
            6), //

    HAIL_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyC.xml",
            "TP",
            41), //

    HAIL_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyC.xml",
            "TP3hr",
            29), //

    HAIL_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyC.xml",
            "TP3hr",
            29), //

    HAIL_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyC.xml",
            "TP6hr",
            15), //

    HAIL_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyC.xml",
            "TP3hr",
            19), //

    HAIL_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HAIL,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyC.xml",
            "TP3hr",
            12), //

    /* Heavy Rain Family */

    HEAVY_RAIN_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/HeavyRainFamily.xml",
            "TP",
            41), //

    HEAVY_RAIN_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.GFS20,
            "bundles/volume/HeavyRainFamily.xml",
            "TP",
            55), //

    HEAVY_RAIN_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/HeavyRainFamily.xml",
            "TP",
            41), //

    HEAVY_RAIN_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.LAPS,
            "bundles/volume/HeavyRainFamily.xml",
            "TP",
            6), //

    HEAVY_RAIN_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.NAM12,
            "bundles/volume/HeavyRainFamily.xml",
            "TP3hr",
            29), //

    HEAVY_RAIN_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.NAM40,
            "bundles/volume/HeavyRainFamily.xml",
            "TP3hr",
            29), //

    HEAVY_RAIN_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.NAM80,
            "bundles/volume/HeavyRainFamily.xml",
            "TP6hr",
            15), //

    HEAVY_RAIN_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.RAP13,
            "bundles/volume/HeavyRainFamily.xml",
            "TP3hr",
            19), //

    HEAVY_RAIN_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.HEAVY_RAIN,
            ModelSourceKind.RAP40,
            "bundles/volume/HeavyRainFamily.xml",
            "TP3hr",
            12), //

    /* MCS Family */

    MCS_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyX.xml",
            "TP",
            41), //

    MCS_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyX.xml",
            "TP",
            55), //

    MCS_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyX.xml",
            "TP",
            41), //

    MCS_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.LAPS,
            "bundles/volume/ModelFamilyX.xml",
            "TP",
            6), //

    MCS_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyX.xml",
            "TP3hr",
            29), //

    MCS_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyX.xml",
            "TP3hr",
            29), //

    MCS_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyX.xml",
            "TP6hr",
            15), //

    MCS_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyX.xml",
            "TP3hr",
            19), //

    MCS_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.MCS,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyX.xml",
            "TP3hr",
            12), //

    /* Non-Supercell Tornado Family */

    NON_SUPERCELL_TOR_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyA.xml",
            "TP",
            41), //

    NON_SUPERCELL_TOR_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyA.xml",
            "TP",
            55), //

    NON_SUPERCELL_TOR_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyA.xml",
            "TP",
            41), //

    NON_SUPERCELL_TOR_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.LAPS,
            "bundles/volume/ModelFamilyA.xml",
            "TP",
            6), //

    NON_SUPERCELL_TOR_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyA.xml",
            "TP3hr",
            29), //

    NON_SUPERCELL_TOR_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyA.xml",
            "TP3hr",
            29), //

    NON_SUPERCELL_TOR_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyA.xml",
            "TP6hr",
            15), //

    NON_SUPERCELL_TOR_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyA.xml",
            "TP3hr",
            19), //

    NON_SUPERCELL_TOR_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.NONSUPERCELL_TORNADO,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyA.xml",
            "TP3hr",
            12), //

    /* QLCS/Wind Family */

    QLCS_WIND_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyH.xml",
            "TP",
            41), //

    QLCS_WIND_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyH.xml",
            "TP",
            55), //

    QLCS_WIND_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyH.xml",
            "TP",
            41), //

    QLCS_WIND_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.LAPS,
            "bundles/volume/ModelFamilyH.xml",
            "TP",
            6), //

    QLCS_WIND_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyH.xml",
            "TP3hr",
            29), //

    QLCS_WIND_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyH.xml",
            "TP3hr",
            29), //

    QLCS_WIND_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyH.xml",
            "TP6hr",
            15), //

    QLCS_WIND_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyH.xml",
            "TP3hr",
            19), //

    QLCS_WIND_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.QLCS_WIND,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyH.xml",
            "TP3hr",
            12), //

    /* Storm Initiation Family */

    STORM_INIT_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyB.xml",
            "TP",
            41), //

    STORM_INIT_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyB.xml",
            "TP",
            55), //

    STORM_INIT_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyB.xml",
            "TP",
            41), //

    STORM_INIT_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.LAPS,
            "bundles/volume/ModelFamilyB.xml",
            "TP",
            6), //

    STORM_INIT_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyB.xml",
            "TP3hr",
            29), //

    STORM_INIT_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyB.xml",
            "TP3hr",
            29), //

    STORM_INIT_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyB.xml",
            "TP6hr",
            15), //

    STORM_INIT_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyB.xml",
            "TP3hr",
            19), //

    STORM_INIT_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_INITIATION,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyB.xml",
            "TP3hr",
            12), //

    /* Storm Type Family */

    STORM_TYPE_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyV.xml",
            "TP",
            41), //

    STORM_TYPE_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyV.xml",
            "TP",
            55), //

    STORM_TYPE_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyV.xml",
            "TP",
            41), //

    STORM_TYPE_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.LAPS,
            "bundles/volume/ModelFamilyV.xml",
            "TP",
            6), //

    STORM_TYPE_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyV.xml",
            "TP3hr",
            29), //

    STORM_TYPE_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyV.xml",
            "TP3hr",
            29), //

    STORM_TYPE_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyV.xml",
            "TP6hr",
            15), //

    STORM_TYPE_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyV.xml",
            "TP3hr",
            19), //

    STORM_TYPE_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.STORM_TYPE,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyV.xml",
            "TP3hr",
            12), //

    /* Supercell Family */

    SUPERCELL_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyW.xml",
            "TP",
            41), //

    SUPERCELL_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyW.xml",
            "TP",
            55), //

    SUPERCELL_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyW.xml",
            "TP",
            41), //

    SUPERCELL_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.LAPS,
            "bundles/volume/ModelFamilyW.xml",
            "TP",
            6), //

    SUPERCELL_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyW.xml",
            "TP3hr",
            29), //

    SUPERCELL_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyW.xml",
            "TP3hr",
            29), //

    SUPERCELL_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyW.xml",
            "TP6hr",
            15), //

    SUPERCELL_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyW.xml",
            "TP3hr",
            19), //

    SUPERCELL_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyW.xml",
            "TP3hr",
            12), //

    /* Supercell Tornado Family */

    SUPERCELL_TOR_ECMWF_HIRES(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyYY.xml",
            "TP",
            41), //

    SUPERCELL_TOR_GFS20(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyYY.xml",
            "TP",
            55), //

    SUPERCELL_TOR_GFS_GLOBAL(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyYY.xml",
            "TP",
            41), //

    SUPERCELL_TOR_LAPS(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.LAPS,
            "bundles/volume/ModelFamilyYY.xml",
            "TP",
            6), //

    SUPERCELL_TOR_NAM12(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyYY.xml",
            "TP3hr",
            29), //

    SUPERCELL_TOR_NAM40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyYY.xml",
            "TP3hr",
            29), //

    SUPERCELL_TOR_NAM80(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyYY.xml",
            "TP6hr",
            15), //

    SUPERCELL_TOR_RAP13(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyYY.xml",
            "TP3hr",
            19), //

    SUPERCELL_TOR_RAP40(
            ModelFamilyType.CONVECTION,
            ModelFamilySubType.SUPERCELL_TORNADO,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyYY.xml",
            "TP3hr",
            12), //

    /* Surface Family */

    SURFACE_ECMWF_HIRES(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/DefaultSurface.xml",
            "TP",
            41), //

    SURFACE_GFS20(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.GFS20,
            "bundles/volume/DefaultSurface.xml",
            "TP",
            55), //

    SURFACE_ARW(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.ARW_EAST,
            "bundles/volume/DefaultSurface.xml",
            "TP",
            49), //

    SURFACE_MMM(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.NMM_EAST,
            "bundles/volume/DefaultSurface.xml",
            "TP",
            49), //

    SURFACE_HRR(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.HRRR,
            "bundles/volume/DefaultSurface.xml",
            "TP",
            16), //

    SURFACE_LAPS(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.LAPS,
            "bundles/volume/DefaultSurface.xml",
            "TP",
            5), //

    SURFACE_NAM12(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.NAM12,
            "bundles/volume/DefaultSurface.xml",
            "TP3hr",
            29), //

    SURFACE_NAM40(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.NAM40,
            "bundles/volume/DefaultSurface.xml",
            "TP3hr",
            21), //

    SURFACE_NAM_NEST(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.NAM_Nest,
            "bundles/volume/DefaultSurface.xml",
            "TP3hr",
            45), //

    SURFACE_RAP13(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.RAP13,
            "bundles/volume/DefaultSurface.xml",
            "TP3hr",
            18), //

    SURFACE_RAP40(
            ModelFamilyType.SURFACE,
            ModelFamilySubType.BASIC_SFC,
            ModelSourceKind.RAP40,
            "bundles/volume/DefaultSurface.xml",
            "TP3hr",
            9), //

    /* Winter Basic Family */

    WINTER_BASIC_ECMWF_HIRES(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.ECMWF_HIRES,
            "bundles/volume/ModelFamilyD-MandatoryLevels.xml",
            "TP",
            41), //

    WINTER_BASIC_GFS20(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.GFS20,
            "bundles/volume/ModelFamilyD.xml",
            "TP",
            55), //

    WINTER_BASIC_GFS_GLOBAL(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.GFS_GLOBAL,
            "bundles/volume/ModelFamilyD.xml",
            "TP",
            41), //

    WINTER_BASIC_AK_NAM12(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.AK_NAM12,
            "bundles/volume/ModelFamilyD.xml",
            "TP3hr",
            29), //

    WINTER_BASIC_NAM12(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.NAM12,
            "bundles/volume/ModelFamilyD.xml",
            "TP3hr",
            29), //

    WINTER_BASIC_AK_NAM40(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.AK_NAM40,
            "bundles/volume/ModelFamilyD.xml",
            "TP6hr",
            21), //

    WINTER_BASIC_NAM40(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.NAM40,
            "bundles/volume/ModelFamilyD.xml",
            "TP3hr",
            21), //

    WINTER_BASIC_NAM80(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.NAM80,
            "bundles/volume/ModelFamilyD.xml",
            "TP6hr",
            15), //

    WINTER_BASIC_RAP13(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.RAP13,
            "bundles/volume/ModelFamilyD.xml",
            "TP3hr",
            19), //

    WINTER_BASIC_RAP40(
            ModelFamilyType.WINTER_MODEL,
            ModelFamilySubType.BASIC,
            ModelSourceKind.RAP40,
            "bundles/volume/ModelFamilyD.xml",
            "TP3hr",
            9); //

    /* Winter Big Fn Family */

    // WINTER_BIG_FN_GFS20(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG_FN, ModelSources.GFS20,
    // "bundles/volume/ModelFamilyI.xml", "TP", 55), //
    //
    // WINTER_BIG_FN_GFS_GLOBAL(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG_FN, ModelSources.GFS_GLOBAL,
    // "bundles/volume/ModelFamilyI.xml", "TP", 41), //
    //
    // WINTER_BIG_FN_NAM12(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG_FN, ModelSources.NAM12,
    // "bundles/volume/ModelFamilyI.xml", "TP3hr", 29), //
    //
    // WINTER_BIG_FN_NAM40(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG_FN, ModelSources.NAM40,
    // "bundles/volume/ModelFamilyI.xml", "TP3hr", 21), //
    //
    // WINTER_BIG_FN_NAM80(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG_FN, ModelSources.NAM80,
    // "bundles/volume/ModelFamilyI.xml", "TP6hr", 15), //
    //
    // WINTER_BIG_FN_RAP13(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG_FN, ModelSources.RAP13,
    // "bundles/volume/ModelFamilyI.xml", "TP3hr", 19), //
    //
    // WINTER_BIG_FN_RAP(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG_FN,
    // ModelSources.RAP40, "bundles/volume/ModelFamilyI.xml", "TP3hr", 9), //

    /* Winter Big3 Qn Family */

    // WINTER_BIG3_QN_GFS20(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG3_QN, ModelSources.GFS20,
    // "bundles/volume/ModelFamilyM.xml", "TP", 55), //
    //
    // WINTER_BIG3_QN_GFS_GLOBAL(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG3_QN, ModelSources.GFS_GLOBAL,
    // "bundles/volume/ModelFamilyM.xml", "TP", 41), //
    //
    // WINTER_BIG3_QN_NAM12(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG3_QN, ModelSources.NAM12,
    // "bundles/volume/ModelFamilyM.xml", "TP3hr", 29), //
    //
    // WINTER_BIG3_QN_NAM40(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG3_QN, ModelSources.NAM40,
    // "bundles/volume/ModelFamilyM.xml", "TP3hr", 21), //
    //
    // WINTER_BIG3_QN_NAM80(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG3_QN, ModelSources.NAM80,
    // "bundles/volume/ModelFamilyM.xml", "TP6hr", 15), //
    //
    // WINTER_BIG3_QN_RAP13(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG3_QN, ModelSources.RAP13,
    // "bundles/volume/ModelFamilyM.xml", "TP3hr", 19), //
    //
    // WINTER_BIG3_QN_RAP(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.BIG3_QN, ModelSources.RAP40,
    // "bundles/volume/ModelFamilyM.xml", "TP3hr", 9); //

    /* Winter PType Family */

    // WINTER_PTYPE_ECMWF_HIRES(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PTYPE, ModelSources.ECMWF_HIRES,
    // "bundles/volume/ModelFamilyL-MandatoryLevels.xml", "TP", 41), //
    //
    // WINTER_PTYPE_GFS20(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PTYPE,
    // ModelSources.GFS20, "bundles/volume/ModelFamilyL.xml", "TP", 55), //
    //
    // WINTER_PTYPE_GFS_GLOBAL(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PTYPE, ModelSources.GFS_GLOBAL,
    // "bundles/volume/ModelFamilyL.xml", "TP", 41), //
    //
    // WINTER_PTYPE_LAPS(ModelFamilyType.WINTER_MODEL, ModelFamilySubType.PTYPE,
    // ModelSources.LAPS, "bundles/volume/ModelFamilyL.xml", "TP", 5), //
    //
    // WINTER_PTYPE_NAM12(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PTYPE,
    // ModelSources.NAM12, "bundles/volume/ModelFamilyL.xml", "TP3hr", 29), //
    //
    // WINTER_PTYPE_NAM40(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PTYPE,
    // ModelSources.NAM40, "bundles/volume/ModelFamilyL.xml", "TP3hr", 21), //
    //
    // WINTER_PTYPE_NAM80(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PTYPE,
    // ModelSources.NAM80, "bundles/volume/ModelFamilyL.xml", "TP6hr", 15), //
    //
    // WINTER_PTYPE_RAP13(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PTYPE,
    // ModelSources.RAP13, "bundles/volume/ModelFamilyL.xml", "TP3hr", 19), //
    //
    // WINTER_PTYPE_RAP(ModelFamilyType.WINTER_MODEL, ModelFamilySubType.PTYPE,
    // ModelSources.RAP40, "bundles/volume/ModelFamilyL.xml", "TP3hr", 9), //

    /* Winter Snow Growth Family */

    // WINTER_SNOW_GROWTH_GFS20(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.SNOW_GROWTH, ModelSources.GFS20,
    // "bundles/volume/ModelFamilyJ.xml", "TP", 55), //
    //
    // WINTER_SNOW_GROWTH_GFS_GLOBAL(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.SNOW_GROWTH, ModelSources.GFS_GLOBAL,
    // "bundles/volume/ModelFamilyJ.xml", "TP", 41), //
    //
    // WINTER_SNOW_GROWTH_NAM12(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.SNOW_GROWTH, ModelSources.NAM12,
    // "bundles/volume/ModelFamilyJ.xml", "TP3hr", 29), //
    //
    // WINTER_SNOW_GROWTH_NAM40(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.SNOW_GROWTH, ModelSources.NAM40,
    // "bundles/volume/ModelFamilyJ.xml", "TP3hr", 21), //
    //
    // WINTER_SNOW_GROWTH_NAM80(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.SNOW_GROWTH, ModelSources.NAM80,
    // "bundles/volume/ModelFamilyJ.xml", "TP6hr", 15), //
    //
    // WINTER_SNOW_GROWTH_RAP13(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.SNOW_GROWTH, ModelSources.RAP13,
    // "bundles/volume/ModelFamilyJ.xml", "TP3hr", 19), //
    //
    // WINTER_SNOW_GROWTH_RAP(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.SNOW_GROWTH, ModelSources.RAP40,
    // "bundles/volume/ModelFamilyJ.xml", "TP3hr", 9);//

    /* Winter PV-Trop Family */

    // WINTER_PV_TROP_GFS20(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PV_TROP, ModelSources.GFS20,
    // "bundles/volume/ModelFamilyE.xml", "TP", 55), //
    //
    // WINTER_PV_TROP_GFS_GLOBAL(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PV_TROP, ModelSources.GFS_GLOBAL,
    // "bundles/volume/ModelFamilyE.xml", "TP", 41), //
    //
    // WINTER_PV_TROP_NAM12(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PV_TROP, ModelSources.NAM12,
    // "bundles/volume/ModelFamilyE.xml", "TP3hr", 29), //
    //
    // WINTER_PV_TROP_NAM40(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PV_TROP, ModelSources.NAM40,
    // "bundles/volume/ModelFamilyE.xml", "TP3hr", 21), //
    //
    // WINTER_PV_TROP_NAM80(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PV_TROP, ModelSources.NAM80,
    // "bundles/volume/ModelFamilyE.xml", "TP6hr", 15), //
    //
    // WINTER_PV_TROP_RAP13(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PV_TROP, ModelSources.RAP13,
    // "bundles/volume/ModelFamilyE.xml", "TP3hr", 19), //
    //
    // WINTER_PV_TROP_RAP(ModelFamilyType.WINTER_MODEL,
    // ModelFamilySubType.PV_TROP, ModelSources.RAP40,
    // "bundles/volume/ModelFamilyE.xml", "TP3hr", 9), //

    private ModelFamilyType familyType = null;

    private ModelFamilySubType familySubType = null;

    private ModelSourceKind model = null;

    private String familyFileName = null;

    private String totalPrecip = null;

    private int frameCount = 0;

    private ModelFamilyDefinitions(ModelFamilyType modelFamilyType,
            ModelFamilySubType subFamilyType, ModelSourceKind modelFlavor,
            String filename, String totalPrecipitation, int numFrames) {

        familyType = modelFamilyType;
        familySubType = subFamilyType;
        familyFileName = filename;
        totalPrecip = totalPrecipitation;
        frameCount = numFrames;
        model = modelFlavor;
    }

    public String getFamilyFileName() {
        return familyFileName;
    }

    public String getTotalPrecip() {
        return totalPrecip;
    }

    public int getFrameCount() {
        return frameCount;
    }

    public ModelFamilySubType getFamilySubType() {
        return familySubType;
    }

    public ModelFamilyType getFamilyType() {
        return familyType;
    }

    public ModelSourceKind getModelSource() {
        return model;
    }

    public static List<ModelSourceKind> getModelSources(ModelFamilyType mft,
            ModelFamilySubType mfst) {

        List<ModelSourceKind> foundSources = new ArrayList<>();
        ModelFamilyDefinitions[] values = ModelFamilyDefinitions.values();
        for (ModelFamilyDefinitions mfd : values) {
            if (mfd.getFamilyType() == mft && mfd.getFamilySubType() == mfst) {
                foundSources.add(mfd.getModelSource());
            }
        }
        return foundSources;
    }
}
