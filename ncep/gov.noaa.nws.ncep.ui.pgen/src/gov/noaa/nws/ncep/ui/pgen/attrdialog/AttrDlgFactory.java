/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.AttrDlgFactory
 * 
 * 20 February 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.VaaCloudDlg;
import gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.VolcanoVaaAttrDlg;
import gov.noaa.nws.ncep.ui.pgen.elements.Track;

import org.eclipse.swt.widgets.Shell;

/**
 * This class is to create attribute dialogs for PGEN elements.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02/09					B. Yin   	Initial Creation.
 * 04/09		#88			J. Wu  		Added Text.
 * 04/09		#89			J. Wu  		Added Arc.
 * 05/09		#111		J. Wu  		Added Vector.
 * 06/09		#130		J. Wu  		Added Extrapolation.
 * 06/09		#116		B. Yin		Added labeled symbol and volcano
 * 07/09		#135		B. Yin		Added Jet 
 * 07/09        #104        S. Gilbert  Added AvnText
 * 08/09        #142        S. Gilbert  Added Interpolation dialog
 * 10/09		#167		J. Wu  		Added Contours dialog.
 * 10/09		#160		G. Zhang	Added Sigmet dialog
 * 01/10		#182		G. Zhang	Added ConvSigmet dialog
 * 01/10		#104?		S. Gilbert	Added MidLevelCloud dialog
 * 05/10		?			B. Yin		Added Outlook dialog 
 * 07/10		#223		M.Laryukhin	Added GfaFormatAttrDlg
 * 07/10		?			B. Yin		Seperated Front from lines
 * 09/10		#305/306	B. Yin		Added Cloud and Turbulence
 * 02/11        #318        S. Gilbert  Added Distance Display Options Dialog
 * 08/11		#?			B. Yin		Added Pgen Inc/Dec 
 * 06/12        #734        J. Zeng     Add SPENES
 * 11/13        #1065       J. Wu       Added kink lines.
 * </pre>
 * 
 * @author B. Yin
 */

public class AttrDlgFactory {

    /**
     * Creates attribute dialog by invoking the getInstance() method.
     * 
     * @param pgenCategory
     *            Category corresponding to the DE's pgenCategory specifying a
     *            class in the Pgen Palette.
     * @param parShell
     *            parent Shell
     * @return
     */

    public static AttrDlg createAttrDlg(String pgenCategory, String pgenType,
            Shell parShell) {

        if (pgenCategory == null) {
            return null;
        }

        if ((pgenCategory.equalsIgnoreCase("Lines"))
                || ((pgenType != null) && pgenType
                        .equalsIgnoreCase("STATUS_LINE"))) {

            if ((pgenType != null)
                    && (pgenType.equalsIgnoreCase("KINK_LINE_1") || pgenType
                            .equalsIgnoreCase("KINK_LINE_2"))) {
                return KinkLineAttrDlg.getInstance(parShell);
            } else {
                return LineAttrDlg.getInstance(parShell);
            }

        } else if (pgenCategory.equalsIgnoreCase("Front")) {
            return FrontAttrDlg.getInstance(parShell);
        }

        else if ((pgenCategory.equalsIgnoreCase("Marker"))
                || (pgenCategory.equalsIgnoreCase("Symbol"))
                || (pgenCategory.equalsIgnoreCase("Combo"))) {

            if (pgenType != null
                    && (pgenType.equalsIgnoreCase("PRESENT_WX_201"))) {
                return VolcanoAttrDlg.getInstance(parShell);
            } else {
                AttrDlg symDlg = LabeledSymbolAttrDlg.getInstance(parShell);
                symDlg.setPgenCategory(pgenCategory);
                symDlg.setPgenType(pgenType);
                return symDlg;
            }

        }

        else if (pgenCategory.equalsIgnoreCase("Text")) {

            if (pgenType != null && pgenType.equalsIgnoreCase("AVIATION_TEXT")) {
                return AvnTextAttrDlg.getInstance(parShell);
            } else if (pgenType != null
                    && pgenType.equalsIgnoreCase("MID_LEVEL_CLOUD")) {
                return MidLevelCloudAttrDlg.getInstance(parShell);
            } else {
                return TextAttrDlg.getInstance(parShell);
            }

        }

        else if (pgenCategory.equalsIgnoreCase("Arc")) {

            return ArcAttrDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase(Track.TRACK_PGEN_CATEGORY)) {

            return TrackAttrDlg.getInstance(parShell);

        } else if (pgenCategory
                .equalsIgnoreCase(Track.TRACK_INFO_DLG_CATEGORY_NAME)) {

            return TrackExtrapPointInfoDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("Vector")) {

            return VectorAttrDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("Extrap")) {

            return PgenExtrapDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("Interp")) {

            return PgenInterpDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("IncDec")) {

            return IncDecDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("Distance")) {

            return PgenDistanceDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("From")) {

            return FromAttrDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("Cycle")) {

            return CycleDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("Prod_AIRMET")) {

            return GfaFormatAttrDlg.getInstance(parShell);

        } else if (pgenType != null && pgenType.equalsIgnoreCase("Jet")) {

            return JetAttrDlg.getInstance(parShell);
        } else if (pgenType != null && pgenType.equalsIgnoreCase("Outlook")) {
            return OutlookAttrDlg.getInstance(parShell);
        } else if (pgenType != null && pgenType.equalsIgnoreCase("TCA")) {

            return TcaAttrDlg.getInstance(parShell);

        } else if (pgenType != null && pgenType.equalsIgnoreCase("WatchBox")) {

            return WatchBoxAttrDlg.getInstance(parShell);

        } else if ((pgenType != null) && pgenType.equalsIgnoreCase("SPENES")) {
            return SpenesAttrDlg.getInstance(parShell);

        } else if (pgenType != null && pgenType.equalsIgnoreCase("TCM")) {

            return TcmAttrDlg.getInstance(parShell);

        } else if (pgenType != null && pgenType.equalsIgnoreCase("Cloud")) {

            return CloudAttrDlg.getInstance(parShell);

        } else if (pgenType != null && pgenType.equalsIgnoreCase("Turbulence")) {

            return TurbAttrDlg.getInstance(parShell);

        } else if (pgenType != null && pgenType.equalsIgnoreCase("Contours")) {

            return ContoursAttrDlg.getInstance(parShell);

        } else if (pgenCategory.equalsIgnoreCase("SIGMET")) {

            if (pgenType != null && pgenType.equalsIgnoreCase("INTL_SIGMET")) {
                SigmetAttrDlg sigAttrDlg = SigmetAttrDlg.getInstance(parShell);
                sigAttrDlg.setPgenCategory(pgenCategory);
                sigAttrDlg.setPgenType(pgenType);
                return sigAttrDlg;
            } else if (pgenType != null
                    && pgenType.equalsIgnoreCase("VOLC_SIGMET")) {
                VolcanoVaaAttrDlg vaaDlg = VolcanoVaaAttrDlg
                        .getInstance(parShell);
                vaaDlg.setPgenCategory(pgenCategory);
                vaaDlg.setPgenType(pgenType);
                vaaDlg.setFromSelection(true);
                return vaaDlg;
            } else if (pgenType != null
                    && pgenType.equalsIgnoreCase("VACL_SIGMET")) {
                VaaCloudDlg vacDlg = VaaCloudDlg.getInstance(parShell);
                vacDlg.setPgenCategory(pgenCategory);
                vacDlg.setPgenType(pgenType);
                return vacDlg;

            } else if (pgenType != null
                    && pgenType.equalsIgnoreCase("CCFP_SIGMET")) {
                gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.CcfpAttrDlg sigAttrDlg = gov.noaa.nws.ncep.ui.pgen.attrdialog.vaadialog.CcfpAttrDlg
                        .getInstance(parShell);
                sigAttrDlg.setPgenCategory(pgenCategory);
                sigAttrDlg.setPgenType(pgenType);
                return sigAttrDlg;
            } else {// (pgenType != null &&
                    // pgenType.equalsIgnoreCase("CONV_SIGMET")) {
                SigmetCommAttrDlg sigAttrDlg = SigmetCommAttrDlg
                        .getInstance(parShell);
                sigAttrDlg.setPgenCategory(pgenCategory);
                sigAttrDlg.setPgenType(pgenType);
                return sigAttrDlg;
            }
        } else if ("GFA".equalsIgnoreCase(pgenType)) {

            return GfaAttrDlg.getInstance(parShell);

        }

        else
            return null;

    }

}
