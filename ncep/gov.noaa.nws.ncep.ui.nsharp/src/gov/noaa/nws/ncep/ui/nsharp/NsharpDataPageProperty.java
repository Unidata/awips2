package gov.noaa.nws.ncep.ui.nsharp;

/**
 * 
 * gov.noaa.nws.ncep.ui.nsharp.NsharpDataPageProperty
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 06/13/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "NsharpDataPageProperty")
@XmlAccessorType(XmlAccessType.NONE)
public class NsharpDataPageProperty implements ISerializableObject {
    @XmlAttribute
    private int summary1Page = NsharpConstants.PAGE_SUMMARY1;

    @XmlAttribute
    private int summary2Page = NsharpConstants.PAGE_SUMMARY2;

    @XmlAttribute
    private int parcelDataPage = NsharpConstants.PAGE_PARCEL_DATA;

    @XmlAttribute
    private int thermodynamicDataPage = NsharpConstants.PAGE_THERMODYNAMIC_DATA;

    @XmlAttribute
    private int opcDataPage = NsharpConstants.PAGE_OPC_DATA;

    @XmlAttribute
    private int mixingHeightPage = NsharpConstants.PAGE_MIXING_HEIGHT;

    @XmlAttribute
    private int stormRelativePage = NsharpConstants.PAGE_STORM_RELATIVE;

    @XmlAttribute
    private int meanWindPage = NsharpConstants.PAGE_MEAN_WIND;

    @XmlAttribute
    private int convectiveInitiationPage = NsharpConstants.PAGE_CONVECTIVE_INITIATION;

    @XmlAttribute
    private int severePotentialPage = NsharpConstants.PAGE_SEVERE_POTENTIAL;

    // d2dlite
    @XmlAttribute
    private int d2dLitePage = NsharpConstants.PAGE_D2DLITE;

    // d2dlite
    @XmlAttribute
    private int futurePage = NsharpConstants.PAGE_FUTURE;

    @XmlAttribute
    private int numberPagePerDisplay = 1;

    public int getSummary1Page() {
        return summary1Page;
    }

    public void setSummary1Page(int summary1Page) {
        this.summary1Page = summary1Page;
    }

    public int getSummary2Page() {
        return summary2Page;
    }

    public void setSummary2Page(int summary2Page) {
        this.summary2Page = summary2Page;
    }

    public int getParcelDataPage() {
        return parcelDataPage;
    }

    public void setParcelDataPage(int parcelDataPage) {
        this.parcelDataPage = parcelDataPage;
    }

    public int getThermodynamicDataPage() {
        return thermodynamicDataPage;
    }

    public void setThermodynamicDataPage(int thermodynamicDataPage) {
        this.thermodynamicDataPage = thermodynamicDataPage;
    }

    public int getOpcDataPage() {
        return opcDataPage;
    }

    public void setOpcDataPage(int opcDataPage) {
        this.opcDataPage = opcDataPage;
    }

    public int getMixingHeightPage() {
        return mixingHeightPage;
    }

    public void setMixingHeightPage(int mixingHeightPage) {
        this.mixingHeightPage = mixingHeightPage;
    }

    public int getStormRelativePage() {
        return stormRelativePage;
    }

    public void setStormRelativePage(int stormRelativePage) {
        this.stormRelativePage = stormRelativePage;
    }

    public int getMeanWindPage() {
        return meanWindPage;
    }

    public void setMeanWindPage(int meanWindPage) {
        this.meanWindPage = meanWindPage;
    }

    public int getConvectiveInitiationPage() {
        return convectiveInitiationPage;
    }

    public void setConvectiveInitiationPage(int convectiveInitiationPage) {
        this.convectiveInitiationPage = convectiveInitiationPage;
    }

    public int getSeverePotentialPage() {
        return severePotentialPage;
    }

    public void setSeverePotentialPage(int severePotentialPage) {
        this.severePotentialPage = severePotentialPage;
    }

    public int getNumberPagePerDisplay() {
        return numberPagePerDisplay;
    }

    public void setNumberPagePerDisplay(int numberPagePerDisplay) {
        this.numberPagePerDisplay = numberPagePerDisplay;
    }

    // d2dlite
    public int getD2dLitePage() {
        return d2dLitePage;
    }

    public void setD2dLitePage(int d2dLitePage) {
        this.d2dLitePage = d2dLitePage;
    }

    public int getFuturePage() {
        return futurePage;
    }

    public void setFuturePage(int futurePage) {
        this.futurePage = futurePage;
    }
}
