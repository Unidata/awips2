/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.palette;

import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
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
 * This java class performs the RTKP Recent Kp Estimates GUI construction.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#        Engineer    Description
 * ------------  ---------- ----------- -----------------------------------
 * April 4, 2014 1122       sgurung     Initial creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class GeoMagRTKpRecentKpWindow extends ViewPart implements
        SelectionListener, DisposeListener, IPartListener {

    private IWorkbenchPage page;

    private boolean isState = false;

    private Group textGp;

    private StyledText text;

    private Date startTime = null;

    private boolean isEditorVisible = true;

    static final SimpleDateFormat f = new SimpleDateFormat(
            "yyyy-MMM-dd HH:mm:ss");

    /**
     * Constructor
     * 
     */
    public GeoMagRTKpRecentKpWindow() {

        super();
        if (geomagrtkpRecentKpWindow == null)
            geomagrtkpRecentKpWindow = this;
        else {
            GeoMagRTKpRecentKpWindow tmp = geomagrtkpRecentKpWindow;
            geomagrtkpRecentKpWindow = this;
            geomagrtkpRecentKpWindow.setState(tmp.isState());
        }
        f.setTimeZone(TimeZone.getTimeZone("GMT"));

    }

    // create this singleton object
    private static GeoMagRTKpRecentKpWindow geomagrtkpRecentKpWindow = null;

    public static GeoMagRTKpRecentKpWindow getAccess() {
        return geomagrtkpRecentKpWindow;
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

        page = site.getPage();
        page.addPartListener(this);

    }

    /**
     * Disposes resource. invoked by the workbench
     */
    public void dispose() {
        if (!isEditorVisible) {
            return;
        } else {
            super.dispose();
            close();
            geomagrtkpRecentKpWindow = null;

            /*
             * remove the workbench part listener
             */
            page.removePartListener(this);
        }

    }

    private void close() {
        IWorkbenchPage wpage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        if (wpage != null) {
            IViewPart vpart = wpage.findView(RTKpUtil.RECENTKP_VIEW_ID);
            wpage.hideView(vpart);
        }

        NcDisplayMngr.setPanningMode();
    }

    /**
     * Invoked by the workbench, this method sets up the SWT controls for the
     * nctext palette
     */
    @Override
    public void createPartControl(Composite parent) {

        parent.setLayout(new GridLayout(1, false));
        // create textGp group. It contains text and textMode group
        textGp = new Group(parent, SWT.SHADOW_OUT);
        textGp.setLayout(new GridLayout());
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        textGp.setLayoutData(data);

        createTextArea(textGp);
    }

    public void createTextArea(Composite parent) {

        // Text display area
        text = new StyledText(parent, SWT.NONE);

        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        text.setLayoutData(data);
        Font font = text.getFont();
        FontData[] fontData = font.getFontData();
        for (int i = 0; i < fontData.length; i++) {
            fontData[i].setHeight(12);
            fontData[i].setName("courier");
            fontData[i].setStyle(SWT.BOLD);
        }
        Font newFont = new Font(font.getDevice(), fontData);
        text.setFont(newFont);
        text.setEditable(false);
    }

    public void displayRecentKpEstimates(List<String> kp_last10_text,
            List<RGB> kp_last10_color, java.awt.Color titleColor) {

        text.setBackground(new Color(text.getFont().getDevice(), 0, 0, 0));
        text.setText("");

        StyleRange[] styleRanges = new StyleRange[kp_last10_text.size() + 1];

        // set title and title string color
        String title = "Recent Kp Estimates\n";
        text.append(title);
        Color titleStringColor = new Color(text.getFont().getDevice(),
                titleColor.getRed(), titleColor.getGreen(),
                titleColor.getBlue());
        styleRanges[0] = new StyleRange(0, title.length(), titleStringColor,
                null);

        // add recent kp estimates to text display
        if (kp_last10_text != null && kp_last10_color != null) {

            // add kp lines
            for (int i = 0; i < kp_last10_text.size(); i++) {
                String kpStr = kp_last10_text.get(i);
                text.append(kpStr + "\n");
            }

            // add colors
            int strIndex = title.length();
            for (int i = 0; i < kp_last10_text.size(); i++) {
                String kpStr = kp_last10_text.get(i);
                RGB color = kp_last10_color.get(i);
                Color textColor = new Color(text.getFont().getDevice(),
                        color.red, color.green, color.blue);

                int kpStrLength = kpStr.length();

                styleRanges[i + 1] = new StyleRange(strIndex, kpStrLength,
                        textColor, null);

                strIndex += kpStr.length() + 1;

            }
            text.setStyleRanges(styleRanges);

        }
    }

    public StyledText getText() {
        return text;
    }

    public void setText(StyledText text) {
        this.text = text;
    }

    public void widgetDefaultSelected(SelectionEvent se) {

    }

    /*
     * invoked when widget is disposed
     * 
     * @see
     * org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt
     * .events.DisposeEvent)
     */
    public void widgetDisposed(DisposeEvent event) {

    }

    @Override
    public void partActivated(IWorkbenchPart part) {
        if (part instanceof GeoMagRTKpRecentKpWindow) {
            // GeoMagWorldActivityResource rsc =
            // GeoMagWorldActivityResource.getNctextuiResource();
            // if (rsc != null)
            // rsc.setEditable(true);
        }
    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {
        partActivated(part);
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

    /**
     * 
     * @return the currently selected category on the palette
     */
    public String getCurrentCategory() {
        return null;
    }

    @Override
    public void setFocus() {
        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
                .setFocus();
    }

    @Override
    public void widgetSelected(SelectionEvent e) {
    }

    public boolean isState() {
        return isState;
    }

    public void setState(boolean isState) {
        this.isState = isState;
    }

    public void setEditorVisible(boolean isVisible) {
        this.isEditorVisible = isVisible;
    }

    public boolean getEditorVisible() {
        return this.isEditorVisible;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    private Date endTime = null;

    public Date getEndTime() {
        return endTime;
    }

    public void setEndTime(Date endTime) {
        this.endTime = endTime;
    }

}
