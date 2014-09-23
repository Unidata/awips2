package gov.noaa.nws.ncep.viz.rsc.timeseries.view;

import gov.noaa.nws.ncep.common.dataplugin.geomag.GeoMagRecord;
import gov.noaa.nws.ncep.viz.rsc.timeseries.rsc.RetrieveUtils;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

/**
 * 
 * Display sampling readout for GeoMagResource
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 * 07/07/2014    R4079       qzhou     Initial Creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

public class SamplingView extends ViewPart implements SelectionListener,
        DisposeListener, IPartListener {

    public static final String samplingId = "gov.noaa.nws.ncep.viz.rsc.timeseries.view.SamplingView";

    public Composite composite = null;

    private StyledText text;

    private boolean isEditorVisible = true;

    private static SamplingView samplingView = null;

    private Device device;

    private Color textColor;

    private Color currColor;

    private final SimpleDateFormat timeSampleFormat = new SimpleDateFormat(
            "yyyy/MM/dd'T'HH':'mm");

    public static SamplingView getAccess() {
        return samplingView;
    }

    public SamplingView() {

        super();
        if (samplingView == null)
            samplingView = this;
        else {
            SamplingView tmp = samplingView;
            samplingView = this;
        }
    }

    /**
     * Invoked by the workbench to initialize this View.
     */
    public void init(IViewSite site) {
        try {
            super.init(site);

        } catch (PartInitException pie) {

            pie.printStackTrace();
        }

    }

    /**
     * Disposes resource. invoked by the workbench
     */
    public void dispose() {

        if (!isEditorVisible) {
            // GeoMagResource.unregisterMouseHandler();
            return;
        } else {
            super.dispose();

            // NatlCntrsEditor editor = GeoMagResource.getMapEditor();
            //
            // if (editor != null) {
            // for (IRenderableDisplay display : UiUtil
            // .getDisplaysFromContainer(editor)) {
            // // System.out.println("display " + display.toString());
            // for (ResourcePair rp : display.getDescriptor()
            // .getResourceList()) {
            // if (rp.getResource() instanceof GeoMagResource) {
            // GeoMagResource rsc = (GeoMagResource) rp
            // .getResource();
            // rsc.unload();
            // }
            // }
            // }
            // }
            samplingView = null;

            // remove the workbench part listener
            // page.removePartListener(this);
        }

    }

    private void close() {
        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        IViewPart vpart = wpage.findView(samplingId);
        wpage.hideView(vpart);

        NcDisplayMngr.setPanningMode();
    }

    @Override
    public void createPartControl(Composite parent) {

        parent.setLayout(new GridLayout(1, false));

        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 4,
                1));
        composite.setLayout(new GridLayout(2, false));

        createTextArea(composite);

    }

    public void createTextArea(Composite parent) {
        // Text display area
        text = new StyledText(parent, SWT.V_SCROLL | SWT.H_SCROLL);

        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        text.setLayoutData(data);

        Font font = text.getFont();
        FontData[] fontData = font.getFontData();

        for (int i = 0; i < fontData.length; i++) {
            // fontData[i].setHeight(12);
            fontData[i].setName("courier");
            fontData[i].setStyle(1);
        }

        Font newFont = new Font(font.getDevice(), fontData);
        text.setFont(newFont);

        text.setEditable(false);

        device = parent.getDisplay();
        textColor = new Color(device, 0, 0, 0); // RGBColors.getRGBColor("black"));
        currColor = new Color(device, 255, 0, 0); // RGBColors.getRGBColor("red"));

    }

    public void paintSampling(List<GeoMagRecord> recordsList,
            List<Float> hQdcList, List<Float> dQdcList, Calendar cal) {

        StyleRange[] styleRanges = new StyleRange[2];

        List<String[]> list = new ArrayList<String[]>();
        List<GeoMagRecord> geoMagList = getSamplingRecords(recordsList, cal);

        // hdev = h_data91440:2879] - h_qdc
        List<float[]> devList = getSamplingDevs(recordsList, hQdcList,
                dQdcList, cal);

        float[] median = RetrieveUtils.getMedian(recordsList);
        String timeStr = timeSampleFormat.format(RetrieveUtils.getUtcDate(cal));

        text.setText("");
        text.append(" Readout            H     D    HDev  DDev\n");

        for (int i = 0; i < geoMagList.size(); i++) {
            GeoMagRecord geomag = geoMagList.get(i);
            String[] temp = new String[5];

            Calendar calendar = geomag.getDataTime().getValidTime();
            temp[0] = timeSampleFormat.format(RetrieveUtils
                    .getUtcDate(calendar));
            temp[1] = String.format("%6.1f",
                    (geomag.getComponent_1() - median[0]));
            temp[2] = String.format("%6.1f",
                    (geomag.getComponent_2() - median[1]));
            temp[3] = String.format("%6.1f",
                    (geomag.getComponent_1() - devList.get(i)[0]));
            temp[4] = String.format("%6.1f",
                    (geomag.getComponent_2() - devList.get(i)[1]));
            // testing
            // temp[3] = String.format("%.1f", (devList.get(i)[0] - median[0]));
            // temp[4] = String.format("%.1f", (devList.get(i)[1] - median[1]));
            list.add(temp);
        }

        int textLength = 0;
        // print the list.
        for (int i = 0; i < list.size(); i++) {
            String[] drawStr = list.get(i);

            for (int j = 0; j < 5; j++) {
                if (j == 0) {
                    // add indent
                    if (list.get(i)[0].equals(timeStr)) {
                        textLength = text.getText().length();

                        text.append("=>" + drawStr[j]);
                    } else {

                        text.append(" " + drawStr[j]);
                    }
                } else {
                    // text.append(String.format("%6s", drawStr[j]));
                    text.append(drawStr[j]);
                }
            }
            text.append("\n");

        }

        styleRanges[0] = new StyleRange(0, textLength, currColor, null);
        styleRanges[1] = new StyleRange(textLength,
                text.getText().length() + 1, textColor, null);

        // text.setText(text.getText());
        text.setStyleRanges(styleRanges);
        text.setVisible(true);

    }

    public List<float[]> getSamplingDevs(List<GeoMagRecord> recordsList,
            List<Float> hQdcList, List<Float> dQdcList, Calendar cal) {
        List<float[]> devList = new ArrayList<float[]>();

        // according to cal, build displaying List<float[]> devList,
        // which is 14 minutes before and after cal
        Calendar calStart = (Calendar) cal.clone();
        calStart.add(Calendar.MINUTE, -14 - 1); // seconds and round to 1min
        Calendar calEnd = (Calendar) cal.clone();
        calEnd.add(Calendar.MINUTE, 14);

        for (int i = 0; i < recordsList.size(); i++) {
            if (recordsList.get(i).getDataTime().getValidTime()
                    .getTimeInMillis() >= calStart.getTimeInMillis()
                    && recordsList.get(i).getDataTime().getValidTime()
                            .getTimeInMillis() <= calEnd.getTimeInMillis()) {

                float hTemp = hQdcList.get(i);
                float dTemp = dQdcList.get(i);
                devList.add(new float[] { hTemp, dTemp });
            }
        }

        return devList;
    }

    public List<GeoMagRecord> getSamplingRecords(
            List<GeoMagRecord> recordsList, Calendar cal) {

        // according to cal, build displaying List<GeoMagRecord> sampRecord,
        // which is 14 minutes before and after cal
        Calendar calStart = (Calendar) cal.clone();
        calStart.add(Calendar.MINUTE, -14 - 1); // seconds and round to 1min
        Calendar calEnd = (Calendar) cal.clone();
        calEnd.add(Calendar.MINUTE, 14);
        // System.out.println("**start " + calStart + " " + calEnd);

        List<GeoMagRecord> sampRecord = new ArrayList<GeoMagRecord>();
        for (int i = 0; i < recordsList.size(); i++) {
            if (recordsList.get(i).getDataTime().getValidTime()
                    .getTimeInMillis() >= calStart.getTimeInMillis()
                    && recordsList.get(i).getDataTime().getValidTime()
                            .getTimeInMillis() <= calEnd.getTimeInMillis())

                sampRecord.add(recordsList.get(i));
        }

        return sampRecord;
    }

    public StyledText getText() {
        return text;
    }

    public void setText(StyledText text) {
        this.text = text;
    }

    @Override
    public void setFocus() {

    }

    @Override
    public void partActivated(IWorkbenchPart part) {

    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {

    }

    @Override
    public void partClosed(IWorkbenchPart part) {

    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {

    }

    @Override
    public void partOpened(IWorkbenchPart part) {

    }

    @Override
    public void widgetDisposed(DisposeEvent e) {

    }

    @Override
    public void widgetSelected(SelectionEvent e) {

    }

    @Override
    public void widgetDefaultSelected(SelectionEvent e) {

    }

    public void setEditorVisible(boolean isVisible) {
        this.isEditorVisible = isVisible;
    }

    public boolean getEditorVisible() {
        return this.isEditorVisible;
    }
}
