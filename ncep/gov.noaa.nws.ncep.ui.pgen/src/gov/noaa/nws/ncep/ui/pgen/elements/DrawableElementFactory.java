/*
 * DrawableElementFactory
 * 
 * Date created: 15 January 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.elements;

import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.contours.Contours;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.display.ITrack;
import gov.noaa.nws.ncep.ui.pgen.display.IVector.VectorType;
import gov.noaa.nws.ncep.ui.pgen.elements.WatchBox.WatchShape;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.Cloud;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.LabeledLine;
import gov.noaa.nws.ncep.ui.pgen.elements.labeledlines.Turbulence;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.ITcm;
import gov.noaa.nws.ncep.ui.pgen.elements.tcm.TcmFcst;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Ccfp;
import gov.noaa.nws.ncep.ui.pgen.sigmet.ConvSigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.ICcfp;
import gov.noaa.nws.ncep.ui.pgen.sigmet.ISigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.IVaaCloud;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Sigmet;
import gov.noaa.nws.ncep.ui.pgen.sigmet.Volcano;
import gov.noaa.nws.ncep.ui.pgen.sigmet.VolcanoAshCloud;
import gov.noaa.nws.ncep.ui.pgen.tca.TCAElement;

import java.util.ArrayList;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * This factory class is used to create DrawableElement elements from its
 * concrete sub-classes. PGEN can use this factory to create the elements it
 * needs to display without knowing the details of how, as long as the caller
 * implements the IAttribute interface.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01/09					J. Wu   	Initial Creation.
 * 02/09					B. Yin		Added pgenType and locations as parameters
 * 										of the create(...) method. 	
 * 04/09		#88			J. Wu   	Added Text.
 * 04/09		#89			J. Wu   	Added Arc.
 * 05/09        #42         S. Gilbert  Added pgenType and pgenCategory
 * 05/09		#111		J. Wu   	Added Vector.
 * 06/09        #42         S. Gilbert  Added ComboSymbol
 * 07/09		#135		B. Yin		Added Jet and changed DE to ADC
 * 07/09        #104        S. Gilbert  Added AvnText
 * 08/09		#135		B. Yin		Added parameter parent for create()
 * 09/09        #163        S. Gilbert  Added TCA
 * 10/09		#160		G. Zhang	Added Sigmet
 * 01/10		#182		G. Zhang	Added ConvSigmet
 * 01/10		#104?		S. Gilbert	Added Mid Level Cloud
 * 03/10		#223		M.Laryukhin	Gfa added. 
 * 09/10		#304		B. Yin		Added LabeledLine
 * 09/11		?			B. Yin		Added TCM
 * 03/12        #625        S. Gurung   Make Line and Line(Med) thicker for CCFP_SIGMET
 * 04/12        #734        J. Zeng     Added SPENES
 * 11/13/12     #1065       J. Wu       Added kink lines
 * 
 * </pre>
 * 
 * @author J. Wu
 * @version 0.0.1
 */
public class DrawableElementFactory {

    // private final static org.apache.log4j.Logger log =
    // org.apache.log4j.Logger.getLogger(DrawableElementFactory.class);

    public AbstractDrawableComponent create(DrawableType typeName,
            IAttribute attr, String pgenCategory, String pgenType,
            ArrayList<Coordinate> locations, AbstractDrawableComponent parent) {

        AbstractDrawableComponent de = null;

        switch (typeName) {

        case LINE:
            de = new Line();
            Line ln = (Line) de;
            ln.setLinePoints(locations);
            // }
            break;

        case SYMBOL:
            de = new Symbol();
            Symbol sbl = (Symbol) de;
            sbl.setLocation(locations.get(0));
            break;

        case KINKLINE:
            de = new KinkLine();
            KinkLine kln = (KinkLine) de;
            kln.setLinePoints(locations);
            break;

        case TEXT:
            de = new Text();
            Text txt = (Text) de;
            txt.setLocation(locations.get(0));
            break;

        case AVN_TEXT:
            de = new AvnText();
            AvnText avntxt = (AvnText) de;
            avntxt.setLocation(locations.get(0));
            break;

        case MID_CLOUD_TEXT:
            de = new MidCloudText();
            MidCloudText cldtxt = (MidCloudText) de;
            cldtxt.setLocation(locations.get(0));
            break;

        case ARC:
            de = new Arc();
            Arc arc = (Arc) de;
            arc.setLinePoints(locations);
            break;

        case VECTOR:
            de = new Vector();
            Vector vec = (Vector) de;
            vec.setLocation(locations.get(0));

            vec.setDirectionOnly(false);

            if (pgenType.equalsIgnoreCase("Arrow")) {
                vec.setVectorType(VectorType.ARROW);
            } else if (pgenType.equalsIgnoreCase("Barb")) {
                vec.setVectorType(VectorType.WIND_BARB);
            } else if (pgenType.equalsIgnoreCase("Directional")) {
                vec.setVectorType(VectorType.ARROW);
                vec.setDirectionOnly(true);
            } else if (pgenType.equalsIgnoreCase("Hash")) {
                vec.setVectorType(VectorType.HASH_MARK);
            } else {
                vec.setVectorType(VectorType.ARROW);
            }

            break;

        case TRACK:
            de = new Track();
            Track track = (Track) de;

            if (attr instanceof ITrack) {
                track.initializeTrackByTrackAttrDlgAndLocationList(
                        (ITrack) attr, locations);
            }

            break;

        case COMBO_SYMBOL:
            de = new ComboSymbol();
            ComboSymbol combo = (ComboSymbol) de;
            combo.setLocation(locations.get(0));
            break;

        case JET:
            de = new Jet(attr, locations);
            break;
        case TCA:
            de = new TCAElement();
            break;

        case CONTOURS:
            de = new Contours();
            break;

        case SIGMET:
            de = new Sigmet();
            Sigmet sgm = (Sigmet) de;
            sgm.setLinePoints(locations);
            if (attr instanceof ISigmet) {
                sgm.setType(((ISigmet) attr).getLineType());
                sgm.setWidth(((ISigmet) attr).getWidth());// SOL of line ???
            }
            break;

        case CONV_SIGMET:
            de = new ConvSigmet();
            ConvSigmet csgm = (ConvSigmet) de;
            csgm.setLinePoints(locations);

            if (pgenType.equals("CCFP_SIGMET") && attr instanceof ICcfp) {

                ((ICcfp) attr).copyEditableAttrToAbstractSigmet(csgm);
                break;
            }

            csgm.setType(((ISigmet) attr).getLineType());
            csgm.setWidth(((ISigmet) attr).getWidth());// SOL of line ???
            break;

        case VAA:
            de = new Volcano();
            Volcano volc = (Volcano) de;
            volc.setLinePoints(locations);
            break;

        case VAA_CLOUD:
            de = new VolcanoAshCloud();
            VolcanoAshCloud vCloud = (VolcanoAshCloud) de;
            vCloud.setLinePoints(locations);
            if (attr instanceof IVaaCloud) {
                vCloud.setType(((IVaaCloud) attr).getLineType());
                vCloud.setWidth(((IVaaCloud) attr).getWidth());
                vCloud.setEditableAttrFreeText(((IVaaCloud) attr)
                        .getFhrFlDirSpdTxt());
            }
            break;

        case GFA:
            de = new Gfa(attr, locations);
            break;

        case TCM_FCST:
            de = new TcmFcst(locations.get(0), ((ITcm) attr).getFcstHr(),
                    ((ITcm) attr).getWindRadius());
            break;

        case SPENES:
            de = new Spenes();
            ((Spenes) de).setLinePoints(locations);
            break;
        default:
            /*
             * Do nothing.
             */
            // log.info( typeName + " is not supported.");
            break;
        }

        // Set element's Type and Category
        de.setPgenCategory(pgenCategory);
        de.setPgenType(pgenType);
        de.setParent(parent);

        if (de != null && de instanceof DrawableElement) {
            if (attr != null) {
                ((DrawableElement) de).update(attr);
            }

        }

        return de;
    }

    public AbstractDrawableComponent create(DrawableType typeName,
            IAttribute attr, String pgenCategory, String pgenType,
            Coordinate location, AbstractDrawableComponent parent) {

        ArrayList<Coordinate> locations = new ArrayList<Coordinate>();
        locations.add(location);

        return create(typeName, attr, pgenCategory, pgenType, locations, parent);

    }

    public AbstractDrawableComponent create(DrawableType typeName,
            IAttribute attr, String pgenCategory, String pgenType,
            Coordinate[] points, AbstractDrawableComponent parent) {

        ArrayList<Coordinate> locations = new ArrayList<Coordinate>();

        for (int ii = 0; ii < points.length; ii++) {
            locations.add(points[ii]);
        }

        return create(typeName, attr, pgenCategory, pgenType, locations, parent);

    }

    /**
     * Create an outlook that contains the input child.
     * 
     * @param otlkType
     *            - outlook type
     * @param child
     *            - line or other DEs
     * @param dec
     *            - a DECollection to hold the child
     * @param otlk
     *            - current outlook
     * @param layer
     *            - current layer
     * @return - a new outlook
     */
    public Outlook createOutlook(String otlkType,
            AbstractDrawableComponent child, DECollection dec, Outlook otlk) {
        // AbstractDrawableComponent layer){
        /*
         * Outlook is a DECollection that contains lines and texts. It is not a
         * DrawableElemnt, so the generic create() method cannot apply for
         * outlook.
         */

        Outlook newOtlk = null;
        ;
        if (otlk == null) {
            newOtlk = new Outlook("Outlook");
            newOtlk.setOutlookType(otlkType);
            newOtlk.setPgenCategory("MET");
            newOtlk.setPgenType(otlkType);
        } else {
            newOtlk = otlk.copy();
        }

        if (dec != null && child != null) {
            dec.add(child);
            newOtlk.add(dec);
        }

        return newOtlk;
    }

    /**
     * Create a watch box
     * 
     * @param pgenCategory
     * @param pgenType
     * @param ws
     * @param pt0
     * @param pt1
     * @param anchorsInPoly
     * @param attr
     * @return
     */
    public DECollection createWatchBox(String pgenCategory, String pgenType,
            WatchShape ws, Coordinate pt0, Coordinate pt1,
            ArrayList<Station> anchorsInPoly, IAttribute attr) {

        WatchBox watchBox;

        ArrayList<Coordinate> watchPts = null;
        Station anchor1 = WatchBox.getNearestAnchorPt(pt0, anchorsInPoly);
        Station anchor2 = WatchBox.getNearestAnchorPt(pt1, anchorsInPoly);

        if (anchor1 != null && anchor2 != null) {

            watchPts = WatchBox.generateWatchBoxPts(ws, WatchBox.HALF_WIDTH
                    * PgenUtil.SM2M, WatchBox.snapOnAnchor(anchor1, pt0),
                    WatchBox.snapOnAnchor(anchor2, pt1));
        }

        /*
         * Create the WatchBox element and add it to the product
         */
        if (watchPts != null) {
            // watchBox = (WatchBox)def.create( DrawableType.WATCH_BOX,
            // (IAttribute)attrDlg,
            // pgenCategory, pgenType, watchPts, drawingLayer.getActiveLayer());
            watchBox = new WatchBox();

            watchBox.setLinePoints(watchPts);
            watchBox.setAnchors(anchor1, anchor2);
            watchBox.update(attr);

            // Set Type and Category
            watchBox.setPgenCategory(pgenCategory);
            watchBox.setPgenType(pgenType);

            // add the watch box to PGEN resource
            DECollection dec = new DECollection("Watch");
            dec.setPgenType("WatchBox");
            dec.setPgenCategory("MET");
            dec.add(watchBox);

            return dec;
        } else {
            return null;
        }

    }

    /**
     * If ll is null, create a labeled line element. If ll is not null, put a
     * new line into the existing labeledline.
     * 
     * @param pgenCat
     * @param pgentype
     * @param attrDlg
     * @param points
     * @param ll
     * @param parent
     * @return
     */
    public LabeledLine createLabeledLine(String pgenCat, String pgentype,
            IAttribute attrDlg, List<Coordinate> points, LabeledLine ll,
            DECollection parent) {

        // create a line
        Line ln = new Line();
        ln.update(attrDlg);
        ln.setLinePoints(points);
        ln.setPgenCategory("Lines");

        if (pgentype.equalsIgnoreCase("Cloud")) {

            ln.setPgenType("SCALLOPED");
            if (ll == null || !(ll instanceof Cloud)) {
                ll = new Cloud("Cloud");
                ll.setPgenCategory(pgenCat);
                ll.setPgenType(pgentype);
                ll.setParent(parent);
            }
        } else if (pgentype.equalsIgnoreCase("Turbulence")) {

            ln.setPgenType("LINE_DASHED_4");

            if (ll == null || !(ll instanceof Turbulence)) {
                ll = new Turbulence("Turbulence");
                ll.setPgenCategory(pgenCat);
                ll.setPgenType(pgentype);
                ll.setParent(parent);
            }
        } else if ("CCFP_SIGMET".equalsIgnoreCase(pgentype)) {

            Sigmet sig = new Sigmet();

            if (attrDlg instanceof ICcfp) {
                sig.setType(((ICcfp) attrDlg).getCcfpLineType());

                ln.setPgenType(((ICcfp) attrDlg).getCcfpLineType());// "LINE_SOLID");
                ln.setClosed(((ICcfp) attrDlg).isAreaType());// true);//TODO: to
                                                             // be modified
                                                             // according line
                                                             // type: Area,
                                                             // Line, or
                                                             // Line(Med)
                ln.setFilled(((ICcfp) attrDlg).isAreaType());
                if (!((ICcfp) attrDlg).isAreaType())
                    ln.setLineWidth(3.0f);
                else
                    ln.setLineWidth(2.0f);
            }

            if (ll == null || !(ll instanceof Ccfp)) {
                ll = new Ccfp("CCFP_SIGMET");
                ll.setPgenCategory(pgenCat);
                ll.setPgenType(pgentype);
                ll.setParent(parent);
            }
            ((Ccfp) ll).setSigmet(sig);
            ((Ccfp) ll).setAreaLine(ln);
            ((Ccfp) ll).setAttributes(attrDlg);
        }

        ll.addLine(ln);
        return ll;
    }

}
