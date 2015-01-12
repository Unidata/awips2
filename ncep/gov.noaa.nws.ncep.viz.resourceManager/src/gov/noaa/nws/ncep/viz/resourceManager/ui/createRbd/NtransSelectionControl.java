package gov.noaa.nws.ncep.viz.resourceManager.ui.createRbd;

import static java.lang.System.out;
import gov.noaa.nws.ncep.viz.common.display.NcDisplayType;
import gov.noaa.nws.ncep.viz.common.preferences.NcepGeneralPreferencesPage;
import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.gempak.util.GempakGrid;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsRequestableResourceData.TimelineGenMethod;
import gov.noaa.nws.ncep.viz.resources.manager.AttributeSet;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceCategory;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefinition;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceDefnsMngr;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceName;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Data Selection dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 07/23/2014                B. Hebbard  Fork off NTRANS-specific code from ResourceSelectionControl
 * 08/26/2014                B. Hebbard  Adjust metafile column comparator to put latest data at top
 * 09/15/2014                B. Hebbard  At CPC request, persist model selected across dialog close/open 
 *                                       even if resource not preselected (from existing RBD contents).
 *                                       (This now differs from non-NTRANS behavior.)
 * 09/15/2014                B. Hebbard  Remove bogus "km" from product group name if it appears (per CPC)
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1
 */
public class NtransSelectionControl extends ResourceSelectionControl {

    // protected ResourceDefnsMngr rscDefnsMngr;

    // protected Combo filterCombo = null;

    // protected ResourceName seldResourceName = null;

    // protected String seldFilterStr = "";

    // protected static ResourceCategory prevSeldCat =
    // ResourceCategory.NullCategory;

    protected static String prevSelectedModel; // needed ??

    // a map to store the previous selections for each category.
    // protected static HashMap<ResourceCategory, ResourceName>
    // prevCatSeldRscNames;

    // a map to store the previous selections for each type (model)y.
    protected static HashMap<String, ResourceName> prevModelSelectedRscNames;

    // this list must stay in sync with the cycleTimeCombo.
    // protected ArrayList<DataTime> cycleTimes = new ArrayList<DataTime>();

    // protected Composite sel_rsc_comp = null;

    // protected Text seldRscNameTxt = null;

    // protected Label availDataTimeLbl = null;

    // protected Label cycleTimeLbl = null;

    // protected Combo cycleTimeCombo = null;

    // For now only one of these will be visible but we may want to allow both
    // later
    // (and remove the Modify button from the Create RBD tab)
    // protected Button addResourceBtn = null;

    // protected Button replaceResourceBtn = null;

    // protected Boolean replaceBtnVisible;

    // protected Boolean replaceBtnEnabled;

    // protected Button addToAllPanesBtn = null;

    // protected Label rscTypeLbl = null;

    protected Label rscGroupLbl = null;

    protected Label metafileLbl = null;

    protected Label productLbl = null;

    // protected ListViewer rscCatLViewer = null;

    protected ResourceCategory resourceCategory;

    // protected ListViewer rscTypeLViewer = null;

    // protected ListViewer rscGroupLViewer = null;

    protected ListViewer metafileLViewer; // = null; // this makes it fail.
                                          // why??

    protected ListViewer productLViewer; // = null; // this makes it fail. why??

    protected Map<String, ArrayList<String>> metafileToProductsMap = null;

    protected String selectedMetafile = ""; // or null?

    // protected ListViewer rscAttrSetLViewer = null;

    // protected final static int rscListViewerHeight = 220;

    // protected static Rectangle prevShellBounds = new Rectangle(0, 0, 800,
    // 460);

    // protected Boolean showLatestTimes = false;

    // protected Boolean onlyShowResourcesWithData = false;

    // protected Integer maxLengthOfSelectableAttrSets = 0; // used in
    // justifying
    // the
    // times in the
    // attrSetsList

    // public interface IResourceSelectedListener {
    // public void resourceSelected(ResourceName rscName, boolean replace,
    // boolean addAllPanes, boolean done);
    // }

    // protected Set<IResourceSelectedListener> rscSelListeners = new
    // HashSet<IResourceSelectedListener>();

    // protected NcDisplayType seldDisplayType;

    public NtransSelectionControl(Composite parent, Boolean replaceVisible,
            Boolean replaceEnabled, ResourceName initRscName,
            Boolean multiPane, NcDisplayType dispType) throws VizException {
        super(parent); // TODO -- dorky??

        seldDisplayType = dispType;

        showLatestTimes = NmapCommon.getNcepPreferenceStore().getBoolean(
                NcepGeneralPreferencesPage.ShowLatestResourceTimes);
        onlyShowResourcesWithData = false; // NmapCommon.getNcepPreferenceStore().getBoolean(
                                           // NcepGeneralPreferencesPage.OnlyShowResourcesWithData
                                           // );

        rscDefnsMngr = ResourceDefnsMngr.getInstance();

        replaceBtnVisible = replaceVisible;
        replaceBtnEnabled = replaceEnabled;

        if (prevModelSelectedRscNames == null) {
            prevModelSelectedRscNames = new HashMap<String, ResourceName>();
        }

        resourceCategory = rscDefnsMngr.getResourceCategories(false,
                new NcDisplayType[] { seldDisplayType })[0];

        // seldResourceName = new ResourceName();
        // seldResourceName.setRscCategory(resourceCategory);

        sel_rsc_comp = this;

        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = SWT.FILL;
        gd.verticalAlignment = SWT.FILL;
        gd.widthHint = prevShellBounds.width;
        gd.heightHint = prevShellBounds.height;
        sel_rsc_comp.setLayoutData(gd);

        sel_rsc_comp.setLayout(new FormLayout());

        sel_rsc_comp.addListener(SWT.Resize, new Listener() {
            @Override
            public void handleEvent(Event event) {
                prevShellBounds = sel_rsc_comp.getBounds();
            }
        });

        createSelectResourceGroup(multiPane);

        // set up the content providers for the ListViewers
        setContentProviders();
        addSelectionListeners();

        initWidgets(initRscName);
    }

    // create all the widgets in the Resource Selection (top) section of the
    // sashForm.
    //
    protected void createSelectResourceGroup(Boolean multiPane) {

        // @formatter:off
        /*
        rscCatLViewer = new ListViewer(sel_rsc_comp, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL | SWT.H_SCROLL);
        FormData fd = new FormData();// 100, rscListViewerHeight);
        fd.height = rscListViewerHeight;
        fd.top = new FormAttachment(0, 75);
        fd.left = new FormAttachment(0, 10);
        // fd.right = new FormAttachment( 15, 0 );
        fd.right = new FormAttachment(0, 110);
        // fd.height = 10;
        // fd.width = 10;

        // This allows a resize to change the size of the lists.
        fd.bottom = new FormAttachment(100, -125);
        rscCatLViewer.getList().setLayoutData(fd);

        // rscCatLViewer.getList().setVisible(false);

        Label rscCatLbl = new Label(sel_rsc_comp, SWT.NONE);
        rscCatLbl.setText("VANISH!!");
        fd = new FormData();
        fd.left = new FormAttachment(rscCatLViewer.getList(), 0, SWT.LEFT);
        fd.bottom = new FormAttachment(rscCatLViewer.getList(), -3, SWT.TOP);
        rscCatLbl.setLayoutData(fd);
        */
        // @formatter:on

        // first create the lists and then attach the label to the top of them
        rscTypeLViewer = new ListViewer(sel_rsc_comp, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL | SWT.H_SCROLL);
        FormData fd = new FormData();// 100, rscListViewerHeight);
        fd.height = rscListViewerHeight;
        fd.top = new FormAttachment(0, 75);
        fd.left = new FormAttachment(0, 10);
        // fd.right = new FormAttachment( 15, 0 );
        fd.right = new FormAttachment(17, -8);
        // fd.height = 10;
        // fd.width = 10;

        // This allows a resize to change the size of the lists.
        fd.bottom = new FormAttachment(100, -125);
        /*
         * fd = new FormData();// 150, rscListViewerHeight); fd.height =
         * rscListViewerHeight; fd.top = new
         * FormAttachment(rscCatLViewer.getList(), 0, SWT.TOP); fd.left = new
         * FormAttachment(rscCatLViewer.getList(), 8, SWT.RIGHT); fd.right = new
         * FormAttachment(20, 0);
         * 
         * fd.bottom = new FormAttachment(rscCatLViewer.getList(), 0,
         * SWT.BOTTOM);
         */
        rscTypeLViewer.getList().setLayoutData(fd);

        rscTypeLbl = new Label(sel_rsc_comp, SWT.NONE);
        rscTypeLbl.setText("Model");
        fd = new FormData();
        fd.left = new FormAttachment(rscTypeLViewer.getList(), 0, SWT.LEFT);
        fd.bottom = new FormAttachment(rscTypeLViewer.getList(), -3, SWT.TOP);

        rscTypeLbl.setLayoutData(fd);

        filterCombo = new Combo(sel_rsc_comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fd = new FormData();
        fd.width = 130;
        fd.bottom = new FormAttachment(rscTypeLViewer.getList(), -30, SWT.TOP);
        fd.left = new FormAttachment(rscTypeLViewer.getList(), 0, SWT.LEFT);
        filterCombo.setLayoutData(fd);

        Label filt_lbl = new Label(sel_rsc_comp, SWT.NONE);
        filt_lbl.setText("Model Filter");
        fd = new FormData();
        fd.left = new FormAttachment(filterCombo, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(filterCombo, -3, SWT.TOP);
        filt_lbl.setLayoutData(fd);

        // @formatter:off
        /*
        // first create the lists and then attach the label to the top of them
        rscGroupLViewer = new ListViewer(sel_rsc_comp, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL | SWT.H_SCROLL);
        fd = new FormData();// 150, rscListViewerHeight);
        fd.height = rscListViewerHeight;
        fd.top = new FormAttachment(rscTypeLViewer.getList(), 0, SWT.TOP);
        fd.left = new FormAttachment(rscTypeLViewer.getList(), 8, SWT.RIGHT);
        fd.right = new FormAttachment(40, 0);

        fd.bottom = new FormAttachment(rscTypeLViewer.getList(), 0, SWT.BOTTOM);
        rscGroupLViewer.getList().setLayoutData(fd);

        rscGroupLbl = new Label(sel_rsc_comp, SWT.NONE);
        rscGroupLbl.setText("Group - VANISH!!");
        fd = new FormData();
        fd.left = new FormAttachment(rscGroupLViewer.getList(), 0, SWT.LEFT);
        fd.bottom = new FormAttachment(rscGroupLViewer.getList(), -3, SWT.TOP);
        rscGroupLbl.setLayoutData(fd);

*/
        // @formatter:on
        // first create the lists and then attach the label to the top of them
        metafileLViewer = new ListViewer(sel_rsc_comp, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL | SWT.H_SCROLL);
        fd = new FormData();// 150, rscListViewerHeight);
        fd.height = rscListViewerHeight;
        // fd.top = new FormAttachment(rscGroupLViewer.getList(), 0, SWT.TOP);
        // fd.left = new FormAttachment(rscGroupLViewer.getList(), 8,
        // SWT.RIGHT);
        fd.top = new FormAttachment(rscTypeLViewer.getList(), 0, SWT.TOP);
        fd.left = new FormAttachment(17, 0);
        fd.right = new FormAttachment(45, -8);

        // fd.bottom = new FormAttachment(rscGroupLViewer.getList(), 0,
        // SWT.BOTTOM);
        fd.bottom = new FormAttachment(rscTypeLViewer.getList(), 0, SWT.BOTTOM);
        metafileLViewer.getList().setLayoutData(fd);

        metafileLbl = new Label(sel_rsc_comp, SWT.NONE);
        metafileLbl.setText("Metafile Name");
        fd = new FormData();
        fd.left = new FormAttachment(metafileLViewer.getList(), 0, SWT.LEFT);
        fd.bottom = new FormAttachment(metafileLViewer.getList(), -3, SWT.TOP);
        metafileLbl.setLayoutData(fd);

        // first create the lists and then attach the label to the top of them
        productLViewer = new ListViewer(sel_rsc_comp, SWT.SINGLE | SWT.BORDER
                | SWT.V_SCROLL | SWT.H_SCROLL);
        fd = new FormData();// 150, rscListViewerHeight);
        fd.height = rscListViewerHeight;
        fd.top = new FormAttachment(metafileLViewer.getList(), 0, SWT.TOP);
        fd.left = new FormAttachment(45, 0);
        fd.right = new FormAttachment(84, -8);

        fd.bottom = new FormAttachment(metafileLViewer.getList(), 0, SWT.BOTTOM);
        productLViewer.getList().setLayoutData(fd);

        productLbl = new Label(sel_rsc_comp, SWT.NONE);
        productLbl.setText("Product Group");
        fd = new FormData();
        fd.left = new FormAttachment(productLViewer.getList(), 0, SWT.LEFT);
        fd.bottom = new FormAttachment(productLViewer.getList(), -3, SWT.TOP);
        productLbl.setLayoutData(fd);

        rscAttrSetLViewer = new ListViewer(sel_rsc_comp, SWT.SINGLE
                | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        fd = new FormData();// 260, rscListViewerHeight);
        fd.height = rscListViewerHeight;
        fd.top = new FormAttachment(productLViewer.getList(), 0, SWT.TOP);
        fd.left = new FormAttachment(84, 0);
        fd.right = new FormAttachment(100, -10);
        fd.bottom = new FormAttachment(productLViewer.getList(), 0, SWT.BOTTOM);
        rscAttrSetLViewer.getList().setLayoutData(fd);

        Label rscAttrsLbl = new Label(sel_rsc_comp, SWT.NONE);
        rscAttrsLbl.setText("Attributes");
        fd = new FormData();
        fd.left = new FormAttachment(rscAttrSetLViewer.getList(), 0, SWT.LEFT);
        fd.bottom = new FormAttachment(rscAttrSetLViewer.getList(), -3, SWT.TOP);
        rscAttrsLbl.setLayoutData(fd);

        availDataTimeLbl = new Label(sel_rsc_comp, SWT.None);
        availDataTimeLbl.setText("");
        fd = new FormData();
        fd.left = new FormAttachment(rscAttrSetLViewer.getList(), 0, SWT.LEFT);
        fd.top = new FormAttachment(rscAttrSetLViewer.getList(), 5, SWT.BOTTOM);
        fd.right = new FormAttachment(rscAttrSetLViewer.getList(), 0, SWT.RIGHT);
        availDataTimeLbl.setLayoutData(fd);

        seldRscNameTxt = new Text(sel_rsc_comp, SWT.SINGLE | SWT.BORDER
                | SWT.READ_ONLY);
        // fd = new FormData(360,20);
        fd = new FormData();
        // fd.bottom = new FormAttachment( 100, -50 ); // change to
        // addResourceBtn
        fd.top = new FormAttachment(rscTypeLViewer.getList(), 40, SWT.BOTTOM);
        fd.left = new FormAttachment(rscTypeLViewer.getList(), 0, SWT.LEFT);
        fd.right = new FormAttachment(75, 0);
        seldRscNameTxt.setLayoutData(fd);

        Label seld_rsc_name_lbl = new Label(sel_rsc_comp, SWT.None);
        seld_rsc_name_lbl.setText("Selected NTRANS Resource Name");
        fd = new FormData();
        fd.left = new FormAttachment(seldRscNameTxt, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(seldRscNameTxt, -3, SWT.TOP);
        seld_rsc_name_lbl.setLayoutData(fd);

        addResourceBtn = new Button(sel_rsc_comp, SWT.None);

        fd = new FormData();

        if (replaceBtnVisible) {
            fd.top = new FormAttachment(seldRscNameTxt, 20, SWT.BOTTOM);
            fd.right = new FormAttachment(50, -20);
        } else {
            fd.top = new FormAttachment(seldRscNameTxt, 20, SWT.BOTTOM);
            fd.left = new FormAttachment(50, 20);
        }
        // fd.left = new FormAttachment( seldRscNameTxt, 75, SWT.RIGHT );
        // fd.bottom = new FormAttachment( 100, -10 );
        addResourceBtn.setLayoutData(fd);
        addResourceBtn.setText("  Add Resource "); // Add To RBD

        replaceResourceBtn = new Button(sel_rsc_comp, SWT.None);
        fd = new FormData();
        fd.left = new FormAttachment(50, 20);
        fd.top = new FormAttachment(addResourceBtn, 0, SWT.TOP);
        replaceResourceBtn.setLayoutData(fd);
        replaceResourceBtn.setText(" Replace Resource "); // ie Modify

        // both for now unless we change it to be one or the other
        // addResourceBtn.setVisible( !replaceBtnVisible );
        replaceResourceBtn.setVisible(replaceBtnVisible);

        addToAllPanesBtn = new Button(sel_rsc_comp, SWT.CHECK);
        fd = new FormData();
        fd.left = new FormAttachment(seldRscNameTxt, 40, SWT.RIGHT);
        fd.top = new FormAttachment(replaceResourceBtn, 0, SWT.TOP);
        addToAllPanesBtn.setLayoutData(fd);
        addToAllPanesBtn.setText("Add To All Panes");

        addToAllPanesBtn.setVisible(multiPane);

        // allow the user to enter any previous datatime
        cycleTimeCombo = new Combo(sel_rsc_comp, SWT.READ_ONLY);
        fd = new FormData();
        // fd.left = new FormAttachment( addResourceBtn, 30, SWT.RIGHT );
        fd.left = new FormAttachment(80, 0);
        fd.right = new FormAttachment(100, -20);
        // fd.bottom = new FormAttachment( 100, -10 );
        fd.top = new FormAttachment(seldRscNameTxt, 0, SWT.TOP);

        cycleTimeCombo.setLayoutData(fd);

        cycleTimeLbl = new Label(sel_rsc_comp, SWT.None);
        cycleTimeLbl.setText("");
        fd = new FormData();
        fd.left = new FormAttachment(cycleTimeCombo, 0, SWT.LEFT);
        fd.bottom = new FormAttachment(cycleTimeCombo, -3, SWT.TOP);
        cycleTimeLbl.setLayoutData(fd);
    }

    protected void setContentProviders() {

        // input is the rscDefnsMngr and output is a list of categories based
        // on the forecast flag
        // @formatter:off
        /*
        rscCatLViewer.setContentProvider(new IStructuredContentProvider() {
            @Override
            public Object[] getElements(Object inputElement) {
                return rscDefnsMngr.getResourceCategories(false,
                        new NcDisplayType[] { seldDisplayType }); // don't show
                                                                  // disabled
                                                                  // dfns
            }

            @Override
            public void dispose() {
            }

            @Override
            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }
        });
        */
        // @formatter:on

        // rscCatLViewer.setLabelProvider(new LabelProvider() {
        // public String getText(Object element) {
        // ResourceDefinition rd = (ResourceDefinition) element;
        // return (rd == null ? "null" : rd.getResourceDefnName().replace(
        // "_NT", ""));
        // }
        // });

        // order the Categories according to the

        rscTypeLViewer.setContentProvider(new IStructuredContentProvider() {
            @Override
            public Object[] getElements(Object inputElement) {
                // String rscCat = (String)inputElement;
                if (seldResourceName.getRscCategory() != ResourceCategory.NullCategory) {
                    try {
                        List<ResourceDefinition> rscTypes = rscDefnsMngr
                                .getResourceDefnsForCategory(
                                        seldResourceName.getRscCategory(),
                                        seldFilterStr, seldDisplayType, true, // include
                                                                              // generated
                                                                              // types
                                        false); // only include enabled types

                        return rscTypes.toArray();
                    } catch (VizException e) {
                        MessageDialog errDlg = new MessageDialog(NcDisplayMngr
                                .getCaveShell(), "Error", null,
                                "Error getting Resource Types\n"
                                        + e.getMessage(), MessageDialog.ERROR,
                                new String[] { "OK" }, 0);
                        errDlg.open();
                    }
                }
                return new ResourceDefinition[] {};
            }

            @Override
            public void dispose() {
            }

            @Override
            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }
        });

        rscTypeLViewer.setComparator(new ViewerComparator() {

            // TODO : implement this if we want to group definitions according
            // to some meaningful category....
            public int category(Object element) {
                ResourceDefinition rd = (ResourceDefinition) element;
                return (rd.isForecast() ? 1 : 0);
                // return super.category(element);
            }

            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                int catComp = category(e1) - category(e2);
                return (catComp != 0 ? catComp : rscDefnsMngr
                        .getDefaultRscDefnComparator().compare(
                                (ResourceDefinition) e1,
                                (ResourceDefinition) e2));
            }
        });

        rscTypeLViewer.setLabelProvider(new LabelProvider() {
            public String getText(Object element) {
                ResourceDefinition rd = (ResourceDefinition) element;
                // suffix removal must match preselect check in
                // updateResourceTypes()
                return (rd == null ? "null" : rd.getResourceDefnName().replace(
                        "_NT", ""));
            }
        });

        // @formatter:off
        /*
        rscGroupLViewer.setContentProvider(new IStructuredContentProvider() {
            @Override
            public Object[] getElements(Object inputElement) {
                String rscType = seldResourceName.getRscType();

                if (!rscType.isEmpty()) {
                    // if this resource uses attrSetGroups then get get the list
                    // of
                    // groups. (PGEN uses groups but we will list the subTypes
                    // (products)
                    // and not the single PGEN attr set group)
                    if (rscDefnsMngr.doesResourceUseAttrSetGroups(rscType)
                            && !seldResourceName.isPgenResource()) {

                        List<String> rscAttrSetsList = rscDefnsMngr
                                .getAttrSetGroupNamesForResource(rscType);

                        if (rscAttrSetsList != null
                                && !rscAttrSetsList.isEmpty()) {
                            return rscAttrSetsList.toArray();
                        }
                    } else {
                        try {
                            String[] rscGroups = rscDefnsMngr
                                    .getResourceSubTypes(rscType);

                            if (rscGroups != null && rscGroups.length != 0) {
                                return rscGroups;// .toArray();
                            }
                        } catch (VizException e) {
                            MessageDialog errDlg = new MessageDialog(
                                    NcDisplayMngr.getCaveShell(), "Error",
                                    null, "Error getting sub-types\n"
                                            + e.getMessage(),
                                    MessageDialog.ERROR, new String[] { "OK" },
                                    0);
                            errDlg.open();
                        }
                    }
                }
                return new String[] {};
            }

            @Override
            public void dispose() {
            }

            @Override
            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }
        });

        rscGroupLViewer.setComparator(new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                return super.compare(viewer, e1, e2);
            }
        });
        */
        // @formatter:on

        metafileLViewer.setContentProvider(new IStructuredContentProvider() {
            @Override
            public Object[] getElements(Object inputElement) {
                String rscType = seldResourceName.getRscType();

                if (!rscType.isEmpty()) {
                    // if this resource uses attrSetGroups then get get the
                    // list of groups. (PGEN uses groups but we will list
                    // the subTypes (products) and not the single PGEN attr
                    // set group)
                    if (rscDefnsMngr.doesResourceUseAttrSetGroups(rscType)
                            && !seldResourceName.isPgenResource()) {

                        List<String> rscAttrSetsList = rscDefnsMngr
                                .getAttrSetGroupNamesForResource(rscType);

                        if (rscAttrSetsList != null
                                && !rscAttrSetsList.isEmpty()) {
                            return rscAttrSetsList.toArray();
                        }
                    } else {
                        try {
                            String[] rscGroups = rscDefnsMngr
                                    .getResourceSubTypes(rscType);

                            if (rscGroups != null && rscGroups.length != 0) {

                                buildMetafileToProductsMap(rscGroups);

                                return metafileToProductsMap.keySet().toArray(
                                        new String[0]);
                            }
                        } catch (VizException e) {
                            MessageDialog errDlg = new MessageDialog(
                                    NcDisplayMngr.getCaveShell(), "Error",
                                    null, "Error getting sub-types\n"
                                            + e.getMessage(),
                                    MessageDialog.ERROR, new String[] { "OK" },
                                    0);
                            errDlg.open();
                        }
                    }
                }
                buildMetafileToProductsMap(new String[] {}); // (create) empty
                                                             // map
                return new String[] {};
            }

            @Override
            public void dispose() {
            }

            @Override
            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }
        });

        metafileLViewer.setComparator(new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                // Ordering of the metafile name column is a bit more
                // complicated than the other columns, because we want to
                // present files in reverse chronological order so the most
                // recent data appear at the top. But among files representing
                // the same date+time, we want to revert to standard
                // lexicographical ordering.
                if (!(e1 instanceof String && e2 instanceof String)) {
                    return super.compare(viewer, e1, e2);
                } else {
                    // This pattern covers known date-time orderings in metafile
                    // names (modified _ to - as already done by decoder)
                    final Pattern p = Pattern
                            .compile("((\\d\\d){3,4})-?(\\d\\d)?");
                    Matcher m1 = p.matcher((String) e1);
                    Matcher m2 = p.matcher((String) e2);
                    String datetime1 = "";
                    String datetime2 = "";
                    // Must handle multiple matches -- if found,
                    // take the longest match
                    while (m1.find()) {
                        if (m1.group(0).length() >= datetime1.length()) {
                            datetime1 = m1.group(0);
                        }
                    }
                    while (m2.find()) {
                        if (m2.group(0).length() >= datetime2.length()) {
                            datetime2 = m2.group(0);
                        }
                    }
                    if (datetime1.equals(datetime2)) {
                        return super.compare(viewer, e1, e2);
                    } else {
                        return -1 // latest date/time first
                                * super.compare(viewer, datetime1, datetime2);
                    }
                }
            }
        });

        productLViewer.setContentProvider(new IStructuredContentProvider() {
            @Override
            public Object[] getElements(Object inputElement) {

                if (metafileToProductsMap == null || selectedMetafile == null) {
                    return new String[] {};
                }
                // return all product names associated with the selected
                // metafile

                ArrayList<String> products = metafileToProductsMap
                        .get(selectedMetafile);

                if (products == null) {
                    return new String[] {};
                }
                return products.toArray(new String[0]);
            }

            @Override
            public void dispose() {
            }

            @Override
            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }
        });

        productLViewer.setComparator(new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                return super.compare(viewer, e1, e2);
            }
        });

        productLViewer.setLabelProvider(new LabelProvider() {
            public String getText(Object element) {
                String productName = (String) element;
                // TODO investigate following -- already fixed separately?
                // This happens intermittently; band-aid until cause located...
                final String removeMe = "km";
                if (productName.endsWith(removeMe)) {
                    System.out
                            .println("[WARNING:  Caught a productName ending in '"
                                    + removeMe + "']");
                    return productName.substring(0, productName.length() - 2);
                } else {
                    return productName;
                }
            }
        });

        rscAttrSetLViewer.setContentProvider(new IStructuredContentProvider() {
            @Override
            public Object[] getElements(Object inputElement) {

                // if there is a group selected then
                // if( !seldResourceName.getRscGroup().isEmpty() ) {
                // if( rscDefnsMngr.getAttrSetsForResource( seldResourceName ))
                // }

                // if an attrSetGroup is selected, return the attrSets in the
                // group
                if (!seldResourceName.getRscType().isEmpty()) {
                    List<AttributeSet> attrSets = rscDefnsMngr
                            .getAttrSetsForResource(seldResourceName, true);

                    maxLengthOfSelectableAttrSets = 0;

                    for (AttributeSet as : attrSets) {
                        if (as != null
                                && as.getName().length() > maxLengthOfSelectableAttrSets) {
                            maxLengthOfSelectableAttrSets = as.getName()
                                    .length();
                        }
                    }

                    return attrSets.toArray(new AttributeSet[0]);
                }

                return new String[] {};
            }

            @Override
            public void dispose() {
            }

            @Override
            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }
        });

        rscAttrSetLViewer.setComparator(new ViewerComparator() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                AttributeSet a1 = (AttributeSet) e1;
                AttributeSet a2 = (AttributeSet) e2;

                if (a1.getName().equals("default")
                        || a1.getName().equals("standard")) {
                    return -1;
                } else if (a2.getName().equals("default")
                        || a2.getName().equals("standard")) {
                    return 1;
                } else {
                    // super calls getText which can trigger a bunch of
                    // inventory queries in some cases
                    return (a1.getName().compareTo(a2.getName())); // super.compare(viewer,
                                                                   // e1, e2);
                }
            }
        });

        rscAttrSetLViewer.setLabelProvider(new LabelProvider() {
            public String getText(Object element) {
                String attrSetName = ((AttributeSet) element).getName();

                if (attrSetName.endsWith(".attr")) {
                    attrSetName = attrSetName.substring(0,
                            attrSetName.length() - 5);
                }

                ResourceName rscName = new ResourceName(seldResourceName);
                rscName.setRscAttrSetName(attrSetName);

                ResourceDefinition rscDefn = rscDefnsMngr
                        .getResourceDefinition(rscName.getRscType());

                if (rscDefn == null) {
                    return "";
                }
                //
                if (!showLatestTimes ||
                // !onlyShowResourcesWithData ||
                        rscDefn.isForecast()) {
                    return attrSetName;
                }

                while (attrSetName.length() < maxLengthOfSelectableAttrSets) {
                    attrSetName = attrSetName + " ";
                }

                // If we aren't using the inventory then the query is too slow
                // for the gui.
                // TODO : If the inventory doesn't pan out then we could either
                // implement this in another thread and accept the delay or add
                // a
                // 'Check Availability' button.
                //
                if (rscName.isValid() && rscDefn.usesInventory()
                        && rscDefn.getInventoryEnabled()) {

                    try {
                        DataTime latestTime = rscDefn
                                .getLatestDataTime(rscName);

                        if (latestTime.isNull()) {
                            attrSetName = attrSetName + " (No Data)";
                        } else {
                            DataTime refTime = new DataTime(latestTime
                                    .getRefTime());
                            String latestTimeStr = NmapCommon
                                    .getTimeStringFromDataTime(latestTime, "_");

                            attrSetName = attrSetName + " (" + latestTimeStr
                                    + ")";
                        }
                    } catch (VizException vizex) {
                        out.println(vizex.getMessage());
                    }
                }
                return attrSetName;
            }
        });
    }

    protected void buildMetafileToProductsMap(String[] rscGroups) {
        // Given an array of combined metafile_product strings, build map
        // from metafiles to lists of associated products.
        if (metafileToProductsMap == null) {
            metafileToProductsMap = new HashMap<String, ArrayList<String>>();
        } else {
            metafileToProductsMap.clear();
        }
        for (String pairname : rscGroups) {
            String[] splits = pairname.split("_", 2);
            if (splits == null || splits.length < 2) {
                // error
            } else {
                // @formatter:off
                String metafile = splits[0];
                String product  = splits[1];
                // @formatter:on
                ArrayList<String> products = metafileToProductsMap
                        .get(metafile);
                // if map doesn't yet contain an entry (products list) for
                // this
                // metafile, add one
                if (products == null) {
                    products = new ArrayList<String>();
                    metafileToProductsMap.put(metafile, products);
                }
                products.add(product);
            }
        }
    }

    // add all of the listeners for widgets on this dialog
    protected void addSelectionListeners() {

        // @formatter:off
/*
        rscCatLViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        StructuredSelection seld_elem = (StructuredSelection) event
                                .getSelection();
                        ResourceCategory seldCat = (ResourceCategory) seld_elem
                                .getFirstElement();

                        // get the previously selected resource for this
                        // category

                        seldResourceName = new ResourceName();
                        seldResourceName.setRscCategory(seldCat);

                        prevSeldCat = seldResourceName.getRscCategory();

                        // if a resource was previously selected for this
                        // category, select it
                        //
                        if (prevCatSeldRscNames.containsKey(seldCat)) {
                            seldResourceName = prevCatSeldRscNames.get(seldCat);
                        }

                        updateResourceFilters();

                        updateResourceTypes();
                    }
                });
                */
        // @formatter:on

        filterCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                String filtStr = null; // init to no filter

                if (filterCombo.getSelectionIndex() == 0) { // "All"
                    filtStr = "";
                } else {
                    filtStr = filterCombo.getText();
                }

                if (filtStr.equals(seldFilterStr)) {
                    return;
                }
                seldFilterStr = filtStr;

                updateResourceTypes();
            }
        });

        rscTypeLViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        StructuredSelection seld_elem = (StructuredSelection) event
                                .getSelection();
                        String seld_rsc_type = ((ResourceDefinition) seld_elem
                                .getFirstElement()).getResourceDefnName();

                        // formerly on category selection listener
                        seldResourceName = new ResourceName();
                        seldResourceName.setRscCategory(resourceCategory);

                        prevSelectedModel = seld_rsc_type;

                        if (prevModelSelectedRscNames
                                .containsKey(seld_rsc_type)) {
                            seldResourceName = prevModelSelectedRscNames
                                    .get(seld_rsc_type);
                        } else {
                            seldResourceName.setRscType(seld_rsc_type);
                            seldResourceName.setRscGroup("");
                            seldResourceName.setRscAttrSetName("");
                            seldResourceName.setCycleTime(null);
                        }

                        // updateCycleTimes();

                        // updateResourceGroups();

                        updateMetafiles();
                    }
                });

        // @formatter:off
        /*
        rscGroupLViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        StructuredSelection seld_elem = (StructuredSelection) event
                                .getSelection();
                        seldResourceName.setRscGroup((String) seld_elem
                                .getFirstElement());
                        seldResourceName.setRscAttrSetName("");

                        updateResourceAttrSets();
                    }
                });
        */
        // @formatter:on

        metafileLViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        StructuredSelection seld_elem = (StructuredSelection) event
                                .getSelection();
                        selectedMetafile = (String) seld_elem.getFirstElement();
                        seldResourceName.setRscAttrSetName("");

                        updateProducts();
                    }
                });

        productLViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        StructuredSelection seld_elem = (StructuredSelection) event
                                .getSelection();
                        seldResourceName.setRscGroup(selectedMetafile + "_"
                                + (String) seld_elem.getFirstElement());
                        seldResourceName.setRscAttrSetName("");

                        updateResourceAttrSets();
                    }
                });

        rscAttrSetLViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        StructuredSelection seld_elem = (StructuredSelection) event
                                .getSelection();

                        seldResourceName
                                .setRscAttrSetName(((AttributeSet) seld_elem
                                        .getFirstElement()).getName());

                        updateCycleTimes();

                        updateSelectedResource();
                    }
                });

        // get the selected rsc and add to the list.
        // ignoring the cycle time for now.
        //
        addResourceBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                selectResource(false, false);
            }
        });

        // TODO : do we want replace to pop down the dialog?
        replaceResourceBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                selectResource(true, false);
            }
        });

        // a double click will add the resource and close the dialog
        rscAttrSetLViewer.getList().addListener(SWT.MouseDoubleClick,
                new Listener() {
                    public void handleEvent(Event event) {
                        if (addResourceBtn.isVisible()) {
                            selectResource(false, true);
                        } else {
                            selectResource(true, true);
                        }
                    }
                });

        cycleTimeCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent ev) {
                updateSelectedResource();
            }
        });
    }

    // set the initial values of the widgets. (base this on previously selected
    // values??)
    //
    protected void initWidgets(ResourceName initRscName) {

        seldResourceName = new ResourceName(initRscName);
        seldResourceName.setRscCategory(resourceCategory); // NTRANS

        if ((seldResourceName.getRscType() == null || seldResourceName
                .getRscType().isEmpty())
                && (prevSelectedModel != null && !prevSelectedModel.isEmpty())) {
            ResourceName previousRscNameForModel = prevModelSelectedRscNames
                    .get(prevSelectedModel);
            if (previousRscNameForModel != null) {
                seldResourceName = previousRscNameForModel;
            }
        }

        filterCombo.setItems(new String[] { "All" });
        filterCombo.select(0);

        seldFilterStr = "";

        // update the cat list
        /*
         * rscCatLViewer.setInput(rscDefnsMngr); rscCatLViewer.refresh();
         * rscCatLViewer.getList().deselectAll();
         */

        //
        addToAllPanesBtn.setSelection(false);

        if (seldResourceName == null
                || seldResourceName.getRscCategory() == ResourceCategory.NullCategory) {
            return;
        }
        // @formatter:off
        /*
        if (rscTypeLViewer.getList().getSelectionCount() == 0) {
            seldResourceName = new ResourceName();
            seldResourceName.setRscCategory(resourceCategory); // NTRANS
        }
        */
        // @formatter:on

        updateResourceFilters();

        updateResourceTypes();
    }

    // refresh the types list based on the type in the seldResourceName
    // use seldResourceName to select the type
    protected void updateResourceTypes() {

        rscTypeLViewer.setInput(rscDefnsMngr);
        rscTypeLViewer.refresh();

        rscTypeLViewer.getList().deselectAll();

        //
        if (!seldResourceName.getRscType().isEmpty()) {
            // suffix removal must match label provider for this viewer
            String modelName = seldResourceName.getRscType().replace("_NT", "");
            for (int itmIndx = 0; itmIndx < rscTypeLViewer.getList()
                    .getItemCount(); itmIndx++) {
                if (rscTypeLViewer.getList().getItem(itmIndx).equals(modelName)) {
                    rscTypeLViewer.getList().select(itmIndx);
                    break;
                }
            }

            if (rscTypeLViewer.getList().getSelectionCount() == 0) {
                seldResourceName.setRscType("");
                seldResourceName.setRscGroup("");
                seldResourceName.setRscAttrSetName("");
                seldResourceName.setCycleTime(null);
            }
        }

        // if no type is selected or it is not found for some reason, select the
        // first
        // why???
        // @formatter:off
        /*
        if (seldResourceName.getRscType().isEmpty()
                && rscTypeLViewer.getList().getItemCount() > 0) {

            rscTypeLViewer.getList().select(0);
            StructuredSelection seld_elem = (StructuredSelection) rscTypeLViewer
                    .getSelection();
            String rscType = ((ResourceDefinition) seld_elem.getFirstElement())
                    .getResourceDefnName();
            seldResourceName.setRscType(rscType);
            seldResourceName.setRscGroup("");
            seldResourceName.setRscAttrSetName("");
            seldResourceName.setCycleTime(null);
        }
        */

        // updateCycleTimes();

        // updateResourceGroups();

        updateMetafiles();
    }

    // @formatter:off
    /*
    protected void updateResourceGroups() {
        rscGroupLViewer.setInput(rscDefnsMngr);
        rscGroupLViewer.refresh();

        // if there are no groups
        if (rscGroupLViewer.getList().getItemCount() == 0) {
            if (!seldResourceName.getRscGroup().isEmpty()) {
                // ????
                seldResourceName.setRscGroup("");
                seldResourceName.setRscAttrSetName("");
                seldResourceName.setCycleTime(null);
            }
        } else { // there are items in the groups list
                 // if a group has been selected then select it in the list,
                 // otherwise
                 // select the first in the list and update the seldResourceName
                 //
            rscGroupLViewer.getList().deselectAll();

            //
            if (!seldResourceName.getRscGroup().isEmpty()) {
                for (int itmIndx = 0; itmIndx < rscGroupLViewer.getList()
                        .getItemCount(); itmIndx++) {

                    if (rscGroupLViewer.getList().getItem(itmIndx)
                            .equals(seldResourceName.getRscGroup())) {
                        rscGroupLViewer.getList().select(itmIndx);
                        break;
                    }
                }

                if (rscGroupLViewer.getList().getSelectionCount() == 0) {
                    seldResourceName.setRscGroup("");
                    seldResourceName.setRscAttrSetName("");
                }
            }

            // if no type is selected or it is not found for some reason, select
            // the first
            if (seldResourceName.getRscGroup().isEmpty()
                    && rscGroupLViewer.getList().getItemCount() > 0) {

                rscGroupLViewer.getList().select(0);
                StructuredSelection seld_elem = (StructuredSelection) rscGroupLViewer
                        .getSelection();

                seldResourceName.setRscGroup((String) seld_elem
                        .getFirstElement());
                seldResourceName.setRscAttrSetName("");
            }
        }

        updateMetafiles();
    }
    */
    // @formatter:on

    protected void updateMetafiles() {
        metafileLViewer.setInput(rscDefnsMngr);
        metafileLViewer.refresh();
        if (metafileToProductsMap != null) {
        }

        // if there are no metafiles
        if (metafileLViewer.getList().getItemCount() == 0) {
            // if (!seldResourceName.getRscGroup().isEmpty()) {
            // ????
            seldResourceName.setRscGroup("");
            seldResourceName.setRscAttrSetName("");
            seldResourceName.setCycleTime(null);
            // }
        } else { // there are items in the metafiles list
                 // if a metafile has been selected (before?) then select it in
                 // the list,
                 // otherwise
                 // select the first in the list and update the seldResourceName
                 //
            metafileLViewer.getList().deselectAll();

            //
            if (!seldResourceName.getRscGroup().isEmpty()) {
                selectedMetafile = seldResourceName.getRscGroup().split("_")[0];
                for (int itmIndx = 0; itmIndx < metafileLViewer.getList()
                        .getItemCount(); itmIndx++) {

                    if (metafileLViewer.getList().getItem(itmIndx)
                            .equals(selectedMetafile)) {
                        metafileLViewer.getList().select(itmIndx);
                        break;
                    }
                }

                if (metafileLViewer.getList().getSelectionCount() == 0) {
                    seldResourceName.setRscGroup("");
                    seldResourceName.setRscAttrSetName("");
                }
            }

            // if no metafile is selected or it is not found for some reason,
            // select
            // the first -- NO, don't!
            // @formatter:off
            /*
            if (seldResourceName.getRscGroup().isEmpty()
                    && metafileLViewer.getList().getItemCount() > 0) {

                metafileLViewer.getList().select(0);
                StructuredSelection seld_elem = (StructuredSelection) metafileLViewer
                        .getSelection();

                seldResourceName.setRscGroup((String) seld_elem
                        .getFirstElement());
                seldResourceName.setRscAttrSetName("");
            }
            */
            // @formatter:on
        }

        updateProducts();
    }

    protected void updateProducts() {
        productLViewer.setInput(rscDefnsMngr);
        productLViewer.refresh();

        // if there are no groups
        if (productLViewer.getList().getItemCount() == 0) {
            if (!seldResourceName.getRscGroup().isEmpty()) {
                // ????
                seldResourceName.setRscGroup("");
                seldResourceName.setRscAttrSetName("");
                seldResourceName.setCycleTime(null);
            }
        } else { // there are items in the groups list
                 // if a group has been selected then select it in the list,
                 // otherwise
                 // select the first in the list and update the seldResourceName
                 //
            productLViewer.getList().deselectAll();

            //
            if (!seldResourceName.getRscGroup().isEmpty()) {
                String selectedProductName = seldResourceName.getRscGroup()
                        .split("_")[1];
                for (int itmIndx = 0; itmIndx < productLViewer.getList()
                        .getItemCount(); itmIndx++) {

                    if (productLViewer.getList().getItem(itmIndx)
                            .equals(selectedProductName)) {
                        productLViewer.getList().select(itmIndx);
                        break;
                    }
                }

                if (productLViewer.getList().getSelectionCount() == 0) {
                    seldResourceName.setRscGroup("");
                    seldResourceName.setRscAttrSetName("");
                }
            }

            // if no type is selected or it is not found for some reason, select
            // the first -- NO, don't!
            // @formatter:off
            /*
            if (seldResourceName.getRscGroup().isEmpty()
                    && productLViewer.getList().getItemCount() > 0) {

                productLViewer.getList().select(0);
                StructuredSelection seld_elem = (StructuredSelection) productLViewer
                        .getSelection();

                seldResourceName.setRscGroup(selectedMetafile + "_"
                        + (String) seld_elem.getFirstElement());
                seldResourceName.setRscAttrSetName("");
            }
            */
            // @formatter:on
        }

        updateResourceAttrSets();
    }

    protected void updateResourceAttrSets() {
        rscAttrSetLViewer.setInput(rscDefnsMngr);
        // rscAttrSetLViewer.refresh();

        rscAttrSetLViewer.getList().deselectAll();

        //
        if (!seldResourceName.getRscAttrSetName().isEmpty()) {
            for (int itmIndx = 0; itmIndx < rscAttrSetLViewer.getList()
                    .getItemCount(); itmIndx++) {

                AttributeSet attrSet = (AttributeSet) rscAttrSetLViewer
                        .getElementAt(itmIndx);

                if (attrSet.getName().equals(
                        seldResourceName.getRscAttrSetName())) {
                    rscAttrSetLViewer.getList().select(itmIndx);
                    break;
                }
            }

            if (rscAttrSetLViewer.getList().getSelectionCount() == 0) {
                seldResourceName.setRscAttrSetName("");
            }
        }

        // if no attr set is selected or it is not found for some reason, select
        // the first -- OK here
        if (seldResourceName.getRscAttrSetName().isEmpty()
                && rscAttrSetLViewer.getList().getItemCount() > 0) {

            rscAttrSetLViewer.getList().select(0);
            StructuredSelection seld_elem = (StructuredSelection) rscAttrSetLViewer
                    .getSelection();

            seldResourceName.setRscAttrSetName(((AttributeSet) seld_elem
                    .getFirstElement()).getName());
        }

        updateCycleTimes();

        updateSelectedResource();
    }

    // when an attrSetName is selected and resource name, with possible cycle
    // time,
    // is ready for selection
    //
    public void updateSelectedResource() {

        String availMsg = "Data Not Available";

        // enable/disable the Add Resource Button
        // and set the name of the Resource
        boolean enableSelections = true;

        ResourceDefinition rscDefn = rscDefnsMngr
                .getResourceDefinition(seldResourceName.getRscType());

        if (!seldResourceName.isValid() || rscDefn == null) {
            enableSelections = false;
        }

        //
        if (enableSelections) {
            // if( onlyShowResourcesWithData ) {
            try {
                // this call will query just for the inventory params needed to
                // instantiate the resource
                // (ie imageType, productCode...) and not the actual dataTimes.
                // rscDefnsMngr.verifyParametersExist( seldResourceName );

                if (rscDefn.isForecast()) {
                    if (cycleTimes.isEmpty()) {
                        enableSelections = false;
                    }
                } else if (rscDefn.isPgenResource()) {
                    availMsg = "";
                } else if (!rscDefn.isRequestable()) {
                    availMsg = "";
                } else {
                    // If we aren't using the inventory then the query is too
                    // slow for the gui.
                    // TODO : If the inventory doesn't pan out then we could
                    // either
                    // implement this in another thread and accept the delay or
                    // add a
                    // 'Check Availability' button.
                    DataTime latestTime = rscDefn
                            .getLatestDataTime(seldResourceName);

                    if (latestTime == null || latestTime.isNull()) {
                        enableSelections = false;
                    } else {
                        availMsg = "Latest Data: "
                                + NmapCommon.getTimeStringFromDataTime(
                                        latestTime, "/");
                    }
                }
            } catch (VizException vizex) {
                out.println(vizex.getMessage());
                availMsg = "Error getting latest time.";
                enableSelections = false;
            }
            // }
        }

        if (enableSelections) {

            addResourceBtn.setEnabled(true);
            replaceResourceBtn.setEnabled(replaceBtnEnabled);

            if (rscDefn.isForecast()) {

                cycleTimeLbl.setEnabled(true);
                cycleTimeCombo.setEnabled(true);
                cycleTimeLbl.setVisible(true);
                cycleTimeCombo.setVisible(true);

                int seldCycleTimeIndx = cycleTimeCombo.getSelectionIndex(); // Cycle
                                                                            // for
                                                                            // Ensemble

                // TODO : Allow the user to select 'LATEST' specifically
                if (seldCycleTimeIndx == -1) {
                    seldResourceName.setCycleTimeLatest();
                } else if (seldCycleTimeIndx < cycleTimes.size()) {
                    seldResourceName.setCycleTime(cycleTimes
                            .get(seldCycleTimeIndx));
                } else { // shoulndn't happen
                    seldResourceName.setCycleTimeLatest();
                }

                availDataTimeLbl.setVisible(false);
            } else {
                availDataTimeLbl.setVisible(true);
                availDataTimeLbl.setText(availMsg);
            }

            // For now, don't let the user select 'Latest'
            if (seldResourceName.isLatestCycleTime()) {

                addResourceBtn.setEnabled(false);
                replaceResourceBtn.setEnabled(false);
                seldRscNameTxt.setText("");
            } else {
                seldRscNameTxt.setText(seldResourceName.toString());
            }
        } else {
            seldRscNameTxt.setText("");
            addResourceBtn.setEnabled(false);
            replaceResourceBtn.setEnabled(false);

            availDataTimeLbl.setVisible(true);
            availDataTimeLbl.setText(availMsg);

            cycleTimeLbl.setVisible(false);
            cycleTimeCombo.setVisible(false);
        }

        prevModelSelectedRscNames.put(seldResourceName.getRscType(),
                seldResourceName);
        prevSelectedModel = seldResourceName.getRscType();
    }

    // TODO: add a way to let the user specifically choose the "LATEST" cycle
    // time.
    // Currently the user cannot select a forecast resource without selecting an
    // available cycle time.
    //
    public void updateCycleTimes() {
        ResourceDefinition rscDefn = rscDefnsMngr
                .getResourceDefinition(seldResourceName);

        if (rscDefn == null) {
            cycleTimeLbl.setEnabled(false);
            cycleTimeCombo.setEnabled(false);
            return;
        } else {
            cycleTimeLbl.setEnabled(true);
            cycleTimeCombo.setEnabled(true);
            cycleTimeLbl.setVisible(rscDefn.isForecast());
            cycleTimeCombo.setVisible(rscDefn.isForecast());
            cycleTimeLbl.setVisible(false); // TODO - cleanup ; off for NTRANS
            cycleTimeCombo.setVisible(false); // TODO - cleanup ; off for NTRANS
            availDataTimeLbl.setVisible(!rscDefn.isForecast());

            if (!rscDefn.isForecast()) {
                return;
            }
        }

        try {
            // if this is reading from gempak
            //
            // would like to use the constant in NcGridData but E dependency
            // again.
            if (rscDefn.getPluginName().equals(GempakGrid.gempakPluginName)) {
                /*
                 * For a GEMPAK dataSource get gridCycleTimes from the
                 * dataLocation
                 */
                HashMap<String, String> rscParams = rscDefnsMngr
                        .getAllResourceParameters(seldResourceName);

                cycleTimeCombo.removeAll();
                cycleTimes.clear();
                try {
                    String dataLocation = null;
                    try {
                        dataLocation = GempakGrid.getGempakGridPath(rscParams
                                .get("GDFILE"));
                    } catch (VizException e) {
                        throw new VizException(e);
                    }
                    String[] gridCycleTimes = GempakGrid
                            .getGridCycleTimes(dataLocation,
                                    rscParams.get("GDFILE").toLowerCase());
                    for (String gct : gridCycleTimes) {
                        String gct2DataTimeFormat = "20" + gct.substring(0, 2)
                                + "-" + gct.substring(2, 4) + "-"
                                + gct.substring(4, 6) + " "
                                + gct.substring(7, 9) + ":"
                                + gct.substring(9, 11) + ":00.0 ";
                        cycleTimes.add(0, new DataTime(gct2DataTimeFormat));
                        cycleTimeCombo.add(gct, 0);
                    }
                    if (gridCycleTimes.length > 0) {
                        cycleTimeCombo.select(0);
                    }
                } catch (VizException e) {
                    out.println("Error querying cycle times: "
                            + e.getMessage().split(":")[1]);
                }

                return;
            }

            List<DataTime> availableTimes = null;

            // If the timeline is generated using frame intervals from a given
            // reference/cycle time, then get a list of selectable ref times.
            // Ideally this would also specify a way to generate the ref times
            // but its really
            // just for nctaf right now so just do it like taf needs.
            if (rscDefn.getTimelineGenMethod() == TimelineGenMethod.USE_FCST_FRAME_INTERVAL_FROM_REF_TIME) {
                // rscDefn.getPluginName().equals( "nctaf" ) ) {
                // Integer frameIntvl = rscDefn.getFrameSpan() *
                availableTimes = rscDefn.getNormalizedDataTimes(
                        seldResourceName, 24 * 60);
            } else {
                availableTimes = rscDefn.getDataTimes(seldResourceName);
            }

            // save the currently selected cycle time.
            //
            String curSelTime = cycleTimeCombo.getText();

            cycleTimeCombo.removeAll();
            cycleTimes.clear();

            //
            for (int t = availableTimes.size() - 1; t >= 0; t--) {
                DataTime dt = availableTimes.get(t);
                DataTime refTime = new DataTime(dt.getRefTime());

                if (!cycleTimes.contains(refTime)) {
                    cycleTimes.add(refTime);
                    String timeStr = NmapCommon.getTimeStringFromDataTime(dt,
                            "_");
                    cycleTimeCombo.add(timeStr);
                }
            }

            for (int t = 0; t < cycleTimeCombo.getItemCount(); t++) {
                if (cycleTimeCombo.getItem(t).equals(curSelTime)) {
                    cycleTimeCombo.select(t);
                    break;
                }
            }

            if (cycleTimes.isEmpty()) {
                cycleTimeCombo.setVisible(false);
                cycleTimeLbl.setVisible(false);
                availDataTimeLbl.setVisible(true);
                availDataTimeLbl.setText("No Data Available");
            } else if (cycleTimeCombo.getSelectionIndex() == -1) {
                cycleTimeCombo.select(0);
            }

        } catch (VizException ve) {
            MessageDialog errDlg = new MessageDialog(
                    NcDisplayMngr.getCaveShell(), "Error", null,
                    "Error Requesting Cycle Times:" + ve.getMessage(),
                    MessageDialog.ERROR, new String[] { "OK" }, 0);
            errDlg.open();
            return;
        }

        return;
    }

}