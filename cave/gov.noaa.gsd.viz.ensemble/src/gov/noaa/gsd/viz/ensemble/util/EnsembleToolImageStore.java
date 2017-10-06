package gov.noaa.gsd.viz.ensemble.util;

import org.eclipse.swt.graphics.Image;

/**
 * 
 * This is the image store cache for the EnsembleToolViewer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 17, 2015            polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */

public class EnsembleToolImageStore {

    public static Image SELECTION_POINTER_IMG = null;

    public static Image ELEMENT_CONTOUR_IMG = null;

    public static Image ELEMENT_WINDBARB_IMG = null;

    public static Image ELEMENT_IMAGE_IMG = null;

    public static Image ELEMENT_ICON_IMG = null;

    public static Image ELEMENT_ARROW_IMG = null;

    public static Image ELEMENT_STREAMLINE_IMG = null;

    public static Image GEAR_IMG = null;

    public static Image CLEAR_ALL_IMG = null;

    public static Image BULLS_EYE_IMG = null;

    public static Image HELP_IMG = null;

    public static Image ALPHANUMERIC_ASCENDING_SORT_IMG = null;

    public static Image ALPHANUMERIC_DESCENDING_SORT_IMG = null;

    public static Image DOT_IMG = null;

    public static Image FAVORITES_IMG = null;

    public static Image FAVORITES_MINI_IMG = null;

    public static Image VOLUME_BROWSER_IMG = null;

    public static Image MATRIX_BROWSER_IMG = null;

    public static Image MODEL_FAMILIES_IMG = null;

    public static Image POWER_ON_IMG = null;

    public static Image POWER_OFF_IMG = null;

    public static Image DIAGNOSTICS_IMG = null;

    public static Image REMOVE_SELECTED_IMG = null;

    public static Image REMOVE_ALL_IMG = null;

    public static Image NICKNAME_IMG = null;

    public static Image NEW_IMG = null;

    public static Image TAB_LEGENDS_ENABLED_SELECTED_IMG = null;

    public static Image TAB_LEGENDS_DISABLED_SELECTED_IMG = null;

    public static Image TAB_LEGENDS_ENABLED_UNSELECTED_IMG = null;

    public static Image TAB_LEGENDS_DISABLED_IMG = null;

    public static Image TAB_MATRIX_ENABLED_SELECTED_IMG = null;

    public static Image TAB_MATRIX_DISABLED_SELECTED_IMG = null;

    public static Image TAB_MATRIX_ENABLED_UNSELECTED_IMG = null;

    public static Image TAB_MATRIX_DISABLED_IMG = null;

    public static Image TAB_INFO_ENABLED_IMG = null;

    public static Image TAB_INFO_DISABLED_IMG = null;

    public static Image TAB_OPTIONS_ENABLED_IMG = null;

    public static Image TAB_OPTIONS_DISABLED_IMG = null;

    public static Image TAB_DISTR_VIEWER_ENABLED_IMG = null;

    public static Image TAB_DISTR_VIEWER_ENABLED_SELECTED_IMG = null;

    public static Image TAB_DISTR_VIEWER_DISABLED_IMG = null;

    public static Image VISIBILITY_IMG = null;

    public static void constructImages() {

        if (SELECTION_POINTER_IMG == null) {
            SELECTION_POINTER_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/selection-pointer.gif");
        }

        if (ELEMENT_CONTOUR_IMG == null) {
            ELEMENT_CONTOUR_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/element-contour-19x19.gif");
        }

        if (ELEMENT_IMAGE_IMG == null) {
            ELEMENT_IMAGE_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/element-image-19x19.gif");
        }

        if (ELEMENT_ARROW_IMG == null) {
            ELEMENT_ARROW_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/element-arrow-19x19.gif");
        }

        if (ELEMENT_ICON_IMG == null) {
            ELEMENT_ICON_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/element-icons-19x19.gif");
        }

        if (ELEMENT_STREAMLINE_IMG == null) {
            ELEMENT_STREAMLINE_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/element-streamline-19x19.gif");
        }

        if (ELEMENT_WINDBARB_IMG == null) {
            ELEMENT_WINDBARB_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/element-barbs-19x19.gif");
        }

        if (HELP_IMG == null) {
            HELP_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/help.gif");
        }

        if (BULLS_EYE_IMG == null) {
            BULLS_EYE_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/bullseye.gif");
        }

        if (DOT_IMG == null) {
            DOT_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/dot.gif");
        }

        if (NEW_IMG == null) {
            NEW_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/new.gif");
        }

        if (NICKNAME_IMG == null) {
            NICKNAME_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/nickname.gif");
        }

        if (REMOVE_SELECTED_IMG == null) {
            REMOVE_SELECTED_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/remove-selected.gif");
        }

        if (REMOVE_ALL_IMG == null) {
            REMOVE_ALL_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/remove-all.gif");
        }

        if (FAVORITES_IMG == null) {
            FAVORITES_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/favorites.gif");
        }

        if (FAVORITES_MINI_IMG == null) {
            FAVORITES_MINI_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/favorites-label-icon.gif");
        }

        if (DIAGNOSTICS_IMG == null) {
            DIAGNOSTICS_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/diagnostic.gif");
        }

        if (ALPHANUMERIC_ASCENDING_SORT_IMG == null) {
            ALPHANUMERIC_ASCENDING_SORT_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/a-to-z-sort.gif");
        }

        if (ALPHANUMERIC_DESCENDING_SORT_IMG == null) {
            ALPHANUMERIC_DESCENDING_SORT_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/z-to-a-sort.gif");
        }

        if (GEAR_IMG == null) {
            GEAR_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/calculation-gear-29x24.gif");
        }

        if (CLEAR_ALL_IMG == null) {
            CLEAR_ALL_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/clear-all-29x24.gif");
        }

        if (VOLUME_BROWSER_IMG == null) {
            VOLUME_BROWSER_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/vb-dialog-icon-29x24.gif");
        }

        if (MODEL_FAMILIES_IMG == null) {
            MODEL_FAMILIES_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/model-families.gif");
        }

        if (MATRIX_BROWSER_IMG == null) {
            MATRIX_BROWSER_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/matrix-browser-dialog-icon-29x24.gif");
        }

        if (TAB_INFO_ENABLED_IMG == null) {
            TAB_INFO_ENABLED_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/tab-info-enabled.gif");
        }
        if (POWER_ON_IMG == null) {
            POWER_ON_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/power-on-29x24.gif");
        }

        if (POWER_OFF_IMG == null) {
            POWER_OFF_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/power-off-29x24.gif");
        }

        if (TAB_LEGENDS_ENABLED_UNSELECTED_IMG == null) {
            TAB_LEGENDS_ENABLED_UNSELECTED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-legends-enabled-unselected.gif");
        }

        if (TAB_LEGENDS_ENABLED_SELECTED_IMG == null) {
            TAB_LEGENDS_ENABLED_SELECTED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-legends-enabled-selected.gif");
        }

        if (TAB_LEGENDS_DISABLED_SELECTED_IMG == null) {
            TAB_LEGENDS_DISABLED_SELECTED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-legends-disabled-selected.gif");
        }

        if (TAB_LEGENDS_DISABLED_IMG == null) {
            TAB_LEGENDS_DISABLED_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/tab-legends-disabled.gif");
        }

        if (TAB_MATRIX_ENABLED_UNSELECTED_IMG == null) {
            TAB_MATRIX_ENABLED_UNSELECTED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-matrix-enabled-unselected.gif");
        }

        if (TAB_MATRIX_ENABLED_SELECTED_IMG == null) {
            TAB_MATRIX_ENABLED_SELECTED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-matrix-enabled-selected.gif");
        }

        if (TAB_MATRIX_DISABLED_SELECTED_IMG == null) {
            TAB_MATRIX_DISABLED_SELECTED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-matrix-disabled-selected.gif");
        }

        if (TAB_MATRIX_DISABLED_IMG == null) {
            TAB_MATRIX_DISABLED_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/tab-matrix-disabled.gif");
        }

        if (TAB_INFO_ENABLED_IMG == null) {
            TAB_INFO_ENABLED_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/tab-info-enabled.gif");
        }

        if (TAB_INFO_DISABLED_IMG == null) {
            TAB_INFO_DISABLED_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/tab-info-disabled.gif");
        }

        if (TAB_OPTIONS_ENABLED_IMG == null) {
            TAB_OPTIONS_ENABLED_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/tab-prefs-enabled.gif");
        }

        if (TAB_OPTIONS_DISABLED_IMG == null) {
            TAB_OPTIONS_DISABLED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-prefs-disabled.gif");
        }

        if (TAB_DISTR_VIEWER_ENABLED_IMG == null) {
            TAB_DISTR_VIEWER_ENABLED_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble",
                    "icons/tab-distr-viewer-enabled.gif");
        }

        if (TAB_DISTR_VIEWER_ENABLED_SELECTED_IMG == null) {
            TAB_DISTR_VIEWER_ENABLED_SELECTED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-distr-viewer-enabled-selected.gif");
        }

        if (TAB_DISTR_VIEWER_DISABLED_IMG == null) {
            TAB_DISTR_VIEWER_DISABLED_IMG = ImageResourceManager
                    .getPluginImage("gov.noaa.gsd.viz.ensemble",
                            "icons/tab-distr-viewer-disabled.gif");
        }
        if (VISIBILITY_IMG == null) {
            VISIBILITY_IMG = ImageResourceManager.getPluginImage(
                    "gov.noaa.gsd.viz.ensemble", "icons/visibility.gif");
        }

    }

}
