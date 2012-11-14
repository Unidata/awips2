/*
 * IAttribute
 * 
 * Date created: 15 January 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

/**
* Define major types for the drawable elements.
* 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/09					J. Wu   	Initial Creation.
 * 04/09		#88			J. Wu   	Added Text.
 * 04/09		#89			J. Wu   	Added Arc.
 * 05/09		#111		J. Wu   	Added Vector.
 * 06/09		#111		S. Gilbert   	Added COMBO_SYMBOL.
 * 07/09		#135		B. Yin		Added JET
 * 07/09        #104        S. Gilbert  Added AvnText
 * 09/09        #163        S. Gilbert  Added TCA
 * 07/09		#159		B. Yin		Added Watch Box
 * 07/09		#167		J. Wu		Added Contours
 * 10/09		#160		G. Zhang	Added Sigmet
 * 11/10		#182		G. Zhang	Added CONVSIGMET
 * 01/10		#104?		S. Gilbert	Added MID_CLOUD_TEXT
 * 03/10		#223		M.Laryukhin	Gfa added. 
 * 04/12	    #734        J. Zeng     Added Spenes
 * </pre>
 * 
 * @author	J. Wu
 * @version	0.0.1
* 
*/
public enum DrawableType {
    LINE, SYMBOL, KINKLINE, TEXT, AVN_TEXT, MID_CLOUD_TEXT, ARC, TRACK, VECTOR, ANY, COMBO_SYMBOL, JET, TCA, GFA, 
    WATCH_BOX, CONTOURS, SIGMET, CONV_SIGMET, VAA, VAA_CLOUD, TCM_FCST, SPENES;
}
