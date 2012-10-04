/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.aviation.editor;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.configuration.ConfigurationException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.plugin.taf.common.TafRecord;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.viz.aviation.monitor.TafUtil;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr;
import com.raytheon.viz.aviation.resource.ResourceConfigMgr.ResourceTag;
import com.raytheon.viz.avnconfig.ITafSiteConfig;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.avnconfig.TafSiteData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The LoaderDialog class displays the Loader dialog for AvnFPS.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 28 FEB 2008  938         lvenable    Initial creation.
 * 4/1/2008     934         grichard    Added taf site config usage.
 * 5/12/2008    1119        grichard    Load taf into taf editor.
 * 5/29/2008    937         grichard    Taf refactor first cut.
 * 6/16/2008    937         grichard    Improved viewer/editor interaction.
 * 5/6/2009     1982        grichard    Permit selection of multiple products.
 * 7/9/2010     5078        rferrel     Added catches for File Not Found.
 * 7/26/2010    6693        rferrel     Apply now checks for Delay.
 * 9/9/2010     5468        rferrel     Check for no TAF loaded for a site
 * 10/1/2010    4345        rferrel     Made products display the same as AWIPS I
 * 10/04/2012   1229        rferrel     Made non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class LoaderDialog extends CaveSWTDialog {

    /**
     * The callback to use to open the Taf Viewer/Editor.
     */
    private TafViewerEditorDlg tveDlg;

    /**
     * Product List control.
     */
    private List productList;

    private java.util.List<String> productOrderedList;

    /**
     * Site List control
     */
    private List siteList;

    /**
     * Init From combo box.
     */
    private Combo initFromCbo;

    /**
     * Routine radio button.
     */
    private Button routineRdo;

    /**
     * Amendment radio button.
     */
    private Button amendmentRdo;

    /**
     * Delayed radio button.
     */
    private Button delayedRdo;

    /**
     * Correction radio button.
     */
    private Button correctionRdo;

    /**
     * Configured Taf Product Map.
     */
    private Map<String, java.util.List<String>> stationMap;

    private String defaultProduct;

    /**
     * Main composite on the dialog.
     */
    private Composite mainComp;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public LoaderDialog(Shell parent, TafViewerEditorDlg tveDlg) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT
                | CAVE.DO_NOT_BLOCK);
        setText("AvnFPS Loader");

        this.tveDlg = tveDlg;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(mainLayout);

        // Load the Taf Site Configuration Information.
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            stationMap = config.getAllProducts();
            productOrderedList = config.getProductList();
            defaultProduct = config.getDefaultProduct();
        } catch (FileNotFoundException e) {

        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
        }

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the controls on the display.
     */
    private void initializeComponents() {
        ResourceConfigMgr configMgr = ResourceConfigMgr.getInstance();
        configMgr.setDefaultColors(mainComp);

        createTopLoaderControls(configMgr);

        createBottomButtons(configMgr);
    }

    /**
     * Create the controls at the top of the display.
     */
    private void createTopLoaderControls(ResourceConfigMgr configMgr) {
        Composite topComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        topComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        topComp.setLayoutData(gd);

        configMgr.setDefaultColors(topComp);

        createLeftSideControls(topComp, configMgr);

        createRightSideControls(topComp, configMgr);
    }

    /**
     * Create the controls on the left side of the display.
     * 
     * @param mainComp
     *            Parent composite.
     */
    private void createLeftSideControls(Composite mainComp,
            ResourceConfigMgr configMgr) {
        Composite leftComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        leftComp.setLayout(gl);
        configMgr.setDefaultColors(leftComp);

        // -----------------------------------------
        // Create the products label and list
        // -----------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label productLbl = new Label(leftComp, SWT.CENTER);
        configMgr.setDefaultFontAndColors(productLbl, "Products", gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        productList = new List(leftComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        productList.setLayoutData(gd);
        configMgr.setDefaultColors(productList);
        configMgr.setListBoxFont(productList);

        for (String s : productOrderedList) {
            productList.add(s);
        }
        productOrderedList = null;

        int indexOfProduct = Math.max(0, productList.indexOf(defaultProduct));
        productList.setSelection(indexOfProduct);

        productList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                siteList.removeAll();
                for (String prod : productList.getSelection()) {
                    for (String site : stationMap.get(prod)) {
                        siteList.add(site);
                    }
                }
                siteList.selectAll();
            }
        });

        // ---------------------------------------------------
        // Create the "initialize from" label and controls
        // ---------------------------------------------------
        Composite initComp = new Composite(leftComp, SWT.NONE);
        gl = new GridLayout(2, false);
        initComp.setLayout(gl);
        configMgr.setDefaultColors(initComp);

        Label initFromLbl = new Label(initComp, SWT.CENTER);
        initFromLbl.setText("Initialize from: ");
        configMgr.setDefaultFontAndColors(initFromLbl);

        initFromCbo = new Combo(initComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        configMgr.setDefaultFontAndColors(initFromCbo);
        initFromCbo.add("latest");
        initFromCbo.add("template");
        initFromCbo.add("merge");

        int index = initFromCbo.indexOf(configMgr
                .getDataAsString(ResourceTag.LoadOrder));
        initFromCbo.select(index);

        // ----------------------------------------------------
        // Create the forecast type group
        // ----------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group forecastGroup = new Group(leftComp, SWT.NONE);
        gl = new GridLayout(1, false);
        forecastGroup.setLayout(gl);
        forecastGroup.setLayoutData(gd);
        forecastGroup.setText(" Forecast Type ");
        configMgr.setDefaultFontAndColors(forecastGroup);

        routineRdo = new Button(forecastGroup, SWT.RADIO);
        routineRdo.setText("Routine");
        routineRdo.setSelection(true);
        configMgr.setDefaultFontAndColors(routineRdo);

        amendmentRdo = new Button(forecastGroup, SWT.RADIO);
        amendmentRdo.setText("Amendment");
        configMgr.setDefaultFontAndColors(amendmentRdo);

        delayedRdo = new Button(forecastGroup, SWT.RADIO);
        delayedRdo.setText("Delayed");
        configMgr.setDefaultFontAndColors(delayedRdo);

        correctionRdo = new Button(forecastGroup, SWT.RADIO);
        correctionRdo.setText("Correction");
        configMgr.setDefaultFontAndColors(correctionRdo);
    }

    /**
     * Create the Site control of the right side of the dialog.
     * 
     * @param mainComp
     *            Parent composite.
     */
    private void createRightSideControls(Composite mainComp,
            ResourceConfigMgr configMgr) {
        Composite rightComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        rightComp.setLayout(gl);
        GridData gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        rightComp.setLayoutData(gd);
        configMgr.setDefaultColors(rightComp);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label siteLbl = new Label(rightComp, SWT.CENTER);
        configMgr.setDefaultFontAndColors(siteLbl, "Sites", gd);

        siteList = new List(rightComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        configMgr.setDefaultColors(siteList);
        configMgr.setListBoxFont(siteList);

        // Automatically select the list of sites associated with the product
        // list that is currently selected.
        for (String prod : productList.getSelection()) {
            for (String site : stationMap.get(prod)) {
                siteList.add(site);
            }
        }
        siteList.selectAll();
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons(ResourceConfigMgr configMgr) {
        Composite centeredComp = new Composite(mainComp, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);
        configMgr.setDefaultColors(centeredComp);

        gd = new GridData(90, SWT.DEFAULT);
        Button okBtn = new Button(centeredComp, SWT.NONE);
        configMgr.setDefaultFontAndColors(okBtn, "WWWWW", gd);
        okBtn.setText("OK");
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                apply();
                tveDlg.showDialog();
                shell.dispose();
            }
        });

        gd = new GridData(90, SWT.DEFAULT);
        Button applyBtn = new Button(centeredComp, SWT.NONE);
        configMgr.setDefaultFontAndColors(applyBtn, "WWWWW", gd);
        applyBtn.setText("Apply");
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                apply();
            }
        });

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        configMgr.setDefaultFontAndColors(closeBtn, "WWWWW", gd);
        closeBtn.setText("Close");
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Get proper bbb based on Radio button TAF tag.
     * 
     * @return bbb
     */
    private String tagToBBB() {
        String bbb = "   ";
        if (amendmentRdo.getSelection()) {
            bbb = "AAX";
        } else if (correctionRdo.getSelection()) {
            bbb = "CCX";
        } else if (delayedRdo.getSelection()) {
            bbb = "RRX";
        }
        return bbb;
    }

    private static Pattern ddPat = null;

    private static Pattern d2Pat = null;

    private String[] getTafTemplate(String ident, String bbb) {
        String taf = null;
        String errMsg = null;
        try {
            boolean routineTaf = bbb != null && bbb.startsWith(" ");
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            TafSiteData site = config.getSite(ident);
            String tafDuration = site.getTafDuration();
            String iTime = TafUtil.getFmtIssueTime(0L);
            String vTime = TafUtil.getFmtValidTime(bbb, 0L, tafDuration, null);
            String tmplHour = String.format("%02d",
                    TafUtil.startHour(routineTaf, System.currentTimeMillis()));
            LocalizationFile lFile = config.getTemplateFile(ident, tmplHour);
            java.util.List<String> tmplLines = new ArrayList<String>();
            StringBuilder contents = new StringBuilder();
            BufferedReader input = new BufferedReader(new FileReader(new File(
                    lFile.getFile().getAbsolutePath())));
            String line = null;
            while ((line = input.readLine()) != null) {
                tmplLines.add(line);
            }

            String firstLine = tmplLines.remove(0);

            String pattern = "^" + ident
                    + "\\s+[D\\d]{6}Z\\s+[D\\d/]{6,9}\\s.*$";
            if (Pattern.matches(pattern, firstLine) == false) {
                throw new ConfigurationException("Invalid pattern in template");
            }

            String d1 = vTime.substring(0, 2);
            String d2 = vTime.substring(5, 7);
            String[] tokens = firstLine.trim().split(" +");

            String value = d1;
            if (routineTaf) {
                // Want routine TAF for 00 hour to be issued the day before.
                // Assume issue time contains the day before. (Could be previous
                // month day)
                value = iTime.substring(0, 2);
            }

            if (LoaderDialog.ddPat == null) {
                LoaderDialog.ddPat = Pattern.compile("[Dd][Dd1]");
                LoaderDialog.d2Pat = Pattern.compile("[dD]2");
            }

            // Issue time
            tokens[1] = LoaderDialog.ddPat.matcher(tokens[1]).replaceAll(value);

            // Forecast hours
            tokens[2] = LoaderDialog.ddPat.matcher(tokens[2]).replaceAll(d1);
            tokens[2] = LoaderDialog.d2Pat.matcher(tokens[2]).replaceAll(d2);

            contents.setLength(0);
            contents.append("TAF").append(TafUtil.LINE_BREAK);
            for (String token : tokens) {
                contents.append(token).append(" ");
            }
            contents.append(TafUtil.LINE_BREAK);

            // Add rest of template lines.
            while (tmplLines.size() > 0) {
                contents.append(tmplLines.remove(0)).append(TafUtil.LINE_BREAK);
            }

            taf = contents.toString().trim();
            errMsg = null;
        } catch (ConfigurationException e) {
            taf = null;
            errMsg = e.toString();
        } catch (IOException e) {
            taf = null;
            errMsg = e.toString();
        } catch (IndexOutOfBoundsException e) {
            taf = null;
            errMsg = "Invalid pattern in template.";
        }
        return new String[] { taf, errMsg };
    }

    private void apply() {
        StringBuilder sb = new StringBuilder();
        String[] selected = siteList.getSelection();
        String order = initFromCbo.getItem(initFromCbo.getSelectionIndex());
        String wmoId = "";
        String issueTime = "";
        String bbb = tagToBBB();
        String siteId = "";

        for (int i = 0; i < selected.length; i++) {
            String ident = selected[i];
            sb.append(TafUtil.LINE_BREAK);
            sb.append(TafUtil.LINE_BREAK);
            // Always retrieve pending TAF from the queue
            TafRecord taf = null;
            if (order.equals("template") == false) {
                taf = TafUtil.getLatestTaf(ident);
                if (taf == null) {
                    if (i == 0) {
                        tveDlg.setMessageStatusError("No TAF for site " + ident);
                        return;
                    }
                    continue;
                }
            }

            if (taf != null && order.equals("merge") == false) {
                sb.append(TafUtil.safeFormatTaf(taf, false));
            } else {
                String[] results = getTafTemplate(ident, bbb);
                String tmpl = results[0];
                if (tmpl == null) {
                    MessageBox msgBox = new MessageBox(getParent(), SWT.OK);
                    msgBox.setMessage(ident + ": " + results[1]);
                    msgBox.setText("Problem with template");
                    msgBox.open();
                    return;
                }
                if (taf != null && order.equals("merge") == true) {
                    // TODO merge taf and template here
                    String[] tmp = tmpl.split(TafUtil.LINE_BREAK);
                    String sTmpl = tmp[1];
                    Pattern rexp_tmp = Pattern.compile("^" + ident
                            + "\\s+\\d{6}Z\\s+(\\d{4}/\\d{4}|\\d{6})\\s+");
                    sTmpl = rexp_tmp.matcher(sTmpl).replaceFirst("");
                    String fmtTaf = TafUtil.safeFormatTaf(taf, false)
                            .split("=")[0];
                    Pattern rexp_ramd = Pattern
                            .compile("\\s+AMD\\s(NOT\\sSKED|LTD\\sTO)([\\s\\w/]+)?");
                    fmtTaf = rexp_ramd.matcher(fmtTaf).replaceAll("").trim();
                    StringBuilder contents = new StringBuilder(fmtTaf);
                    contents.append(" ").append(sTmpl);
                    for (int index = 2; index < tmp.length; ++index) {
                        contents.append(TafUtil.LINE_BREAK).append(tmp[index]);
                    }
                    sb.append(contents);
                } else {
                    sb.append(tmpl);
                    if (i == 0) {
                        String itime = tmpl.split("\n")[1].split(" +")[1];
                        issueTime = itime.substring(0, 4) + "00";
                    }
                }
            }

            if (i == 0) {
                if (taf != null) {
                    String[] header = taf.getWmoHeader().split(" ");
                    issueTime = header[2];
                }

                /**
                 * DR #5062: It appears that using the header that comes with
                 * the TAF gives a wrong wmoId and siteId; here, it is changed
                 * to use the header from TafSiteConfig; may need further
                 * investigation on this...
                 */
                try {
                    ITafSiteConfig config = TafSiteConfigFactory.getInstance();
                    TafSiteData siteData = config.getSite(ident);
                    wmoId = siteData.wmo.split(" ")[0];
                    siteId = siteData.wmo.split(" ")[1];
                } catch (FileNotFoundException e) {
                    tveDlg.setSendCollective(false);
                } catch (ConfigurationException e) {
                    tveDlg.setSendCollective(false);
                } catch (IOException e) {
                    tveDlg.setSendCollective(false);
                }

                bbb = tagToBBB();
            }
        }

        tveDlg.populateEditorTab(sb.toString().trim(), wmoId, siteId,
                issueTime, bbb);

        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            String collectivePil = config.getProductCollectivePil(productList
                    .getItem(productList.getSelectionIndex()));

            if (collectivePil != null && !collectivePil.equals("")) {
                tveDlg.setSendCollective(true);
            }
        } catch (FileNotFoundException e) {
            tveDlg.setSendCollective(false);
        } catch (ConfigurationException e) {
            tveDlg.setSendCollective(false);
        }
    }
}
