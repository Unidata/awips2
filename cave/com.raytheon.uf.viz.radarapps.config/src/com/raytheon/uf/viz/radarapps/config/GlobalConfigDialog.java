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
package com.raytheon.uf.viz.radarapps.config;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.rcm.config.EndpointConfig;
import com.raytheon.rcm.config.Globals;

public class GlobalConfigDialog extends Dialog {
    Globals globals;

    Text pupIdText;

    Button collectionEnabledCheck;

    Button tdwrCollectionLimitedCheck;

    Text wmoSiteIdText;

    Text edexEndpointText;

    Button decompressProductsCheck;
    
    Text archiveRootText;
    
    Button prefixWithRadarButton;
    
    Text edexJMSText;

    boolean legacyOptionsVisible;
    
    public boolean isLegacyOptionVisible() {
		return legacyOptionsVisible;
	}

	public void setLegacyOptionVisible(boolean legacyOptionVisible) {
		this.legacyOptionsVisible = legacyOptionVisible;
	}

	public GlobalConfigDialog(Shell shell, Globals globals) {
        super(shell);
        setShellStyle((getShellStyle() & ~SWT.APPLICATION_MODAL)
                | SWT.PRIMARY_MODAL);
        this.globals = globals;
    }

    private String safe(String s) {
        return s != null ? s : "";
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite c = (Composite) super.createDialogArea(parent);
        GridLayout gl = (GridLayout) c.getLayout();
        GridData gd;
        gl.numColumns = 3;

        Label l = new Label(c, SWT.LEFT);
        l.setText("PUP ID:");
        pupIdText = new Text(c, SWT.SINGLE | SWT.BORDER);
        pupIdText.setText(Integer.toString(globals.pupID));
        pupIdText.setTextLimit(5);
        pupIdText.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent e) {
                validate();
            }
        });
        gd = new GridData();
        gd.widthHint = convertWidthInCharsToPixels(6);
        pupIdText.setLayoutData(gd);
        l = new Label(c, SWT.LEFT); // pad col 3

        l = new Label(c, SWT.LEFT | SWT.WRAP);
        l.setText("(This setting takes affect the next time RadarServer "
                + "connects to a RPG.)");
        FontData[] fd = l.getFont().getFontData();
        for (FontData fdi : fd)
            fdi.setHeight(fdi.getHeight() - 2);
        Font smallFont = new Font(l.getDisplay(), fd);
        l.setFont(smallFont);

        gd = new GridData();
        gd.horizontalIndent = convertWidthInCharsToPixels(1);
        gd.horizontalSpan = 2;
        l.setLayoutData(gd);
        l = new Label(c, SWT.LEFT); // pad col 3

        l = new Label(c, SWT.LEFT);
        l.setText("Settings for National Collection:");
        gd = new GridData();
        gd.horizontalSpan = 3;
        gd.verticalIndent = convertHeightInCharsToPixels(1);
        l.setLayoutData(gd);

        collectionEnabledCheck = new Button(c, SWT.CHECK);
        collectionEnabledCheck.setText("Allow transmission of products");
        collectionEnabledCheck.setSelection(globals.collectionEnabled);
        gd = new GridData();
        // gd.verticalIndent = convertHeightInCharsToPixels(1);
        gd.horizontalIndent = convertWidthInCharsToPixels(2);
        gd.horizontalSpan = 3;
        collectionEnabledCheck.setLayoutData(gd);

        l = new Label(c, SWT.LEFT | SWT.WRAP);
        l.setText("(This is a general setting.  Transmission "
                + "must still be enabled for specific radars.)");
        l.setFont(smallFont);
        gd = new GridData();
        gd.horizontalIndent = convertWidthInCharsToPixels(3);
        gd.horizontalSpan = 2;
        l.setLayoutData(gd);
        l = new Label(c, SWT.LEFT); // pad col 3

        tdwrCollectionLimitedCheck = new Button(c, SWT.CHECK);
        tdwrCollectionLimitedCheck
                .setText("Filter duplicate elevations from TDWR products");
        tdwrCollectionLimitedCheck.setSelection(globals.tdwrCollectionLimited);
        gd = new GridData();
        // gd.verticalIndent = convertHeightInCharsToPixels(1);
        gd.horizontalIndent = convertWidthInCharsToPixels(2);
        gd.horizontalSpan = 3;
        tdwrCollectionLimitedCheck.setLayoutData(gd);

        l = new Label(c, SWT.LEFT);
        l.setText("WMO Site ID:");
        wmoSiteIdText = new Text(c, SWT.SINGLE | SWT.BORDER);
        wmoSiteIdText.setText(safe(globals.wmoSiteID));
        gd = new GridData();
        gd.horizontalIndent = convertWidthInCharsToPixels(2);
        l.setLayoutData(gd);
        // wmoSiteIdText.setTextLimit(5);
        gd = new GridData();
        gd.widthHint = convertWidthInCharsToPixels(6);
        wmoSiteIdText.setLayoutData(gd);
        l = new Label(c, SWT.LEFT); // pad col 3

        // Region code should be automatic...

        l = new Label(c, SWT.LEFT); // pad row
        gd = new GridData();
        gd.horizontalSpan = 3;
        l.setLayoutData(gd);
        
        EndpointConfig ec = globals.endpointConfig;
        if (ec == null)
        	ec = new EndpointConfig();
        
        Button b;
        
        l = new Label(c, SWT.LEFT);
        l.setText("Data Archive Root:");
        archiveRootText = new Text(c, SWT.SINGLE | SWT.BORDER);
        archiveRootText.setText(safe(ec.getArchiveRoot()));
        gd = new GridData();
        gd.widthHint = convertWidthInCharsToPixels(30);
        archiveRootText.setLayoutData(gd);
        b = new Button(c, SWT.PUSH);
        b.setText("Browse...");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onBrowseArchiveRoot();
            }
        });
        
        prefixWithRadarButton = new Button(c, SWT.CHECK);
        prefixWithRadarButton.setText("Prefix created paths with \"radar/\"");
        prefixWithRadarButton.setSelection(ec.getPrefixPathWithRadar() != null ?
        		ec.getPrefixPathWithRadar() : false);
    	gd = new GridData();
    	gd.horizontalSpan = 3;
    	gd.horizontalIndent = convertWidthInCharsToPixels(2);
    	prefixWithRadarButton.setLayoutData(gd);
        
        l = new Label(c, SWT.LEFT);
        l.setText("EDEX JMS URL:");
        edexJMSText = new Text(c, SWT.SINGLE | SWT.BORDER);
        edexJMSText.setText(safe(ec.getConnectionURL()));
        gd = new GridData();
        gd.widthHint = convertWidthInCharsToPixels(30);
        edexJMSText.setLayoutData(gd);
        
        l = new Label(c, SWT.LEFT); // pad out rest of row
        
        if (legacyOptionsVisible) {
        	l = new Label(c, SWT.LEFT);
        	l.setText("Legacy Ingest Options:");
        	gd = new GridData();
        	gd.horizontalSpan = 3;
        	l.setLayoutData(gd);

        	l = new Label(c, SWT.LEFT);
        	l.setText("EDEX Radar Endpoint:");
        	gd = new GridData();
        	gd.horizontalIndent = convertWidthInCharsToPixels(2);
        	l.setLayoutData(gd);
        	edexEndpointText = new Text(c, SWT.SINGLE | SWT.BORDER);
        	edexEndpointText.setText(safe(globals.edexEndpoint));
        	gd = new GridData();
        	gd.widthHint = convertWidthInCharsToPixels(30);
        	edexEndpointText.setLayoutData(gd);
        	b = new Button(c, SWT.PUSH);
        	b.setText("Browse...");
        	b.addSelectionListener(new SelectionAdapter() {
        		public void widgetSelected(SelectionEvent e) {
        			onBrowseEndpoint();
        		}
        	});

        	decompressProductsCheck = new Button(c, SWT.CHECK);
        	decompressProductsCheck
        	.setText("Decompress products before sending to EDEX");
        	decompressProductsCheck.setSelection(globals.decompressProducts);
        	gd = new GridData();
        	gd.horizontalSpan = 3;
        	gd.horizontalIndent = convertWidthInCharsToPixels(2);
        	decompressProductsCheck.setLayoutData(gd);
        }

        smallFont.dispose();

        return c;
    }

    private void validate() {
        Button b = getButton(IDialogConstants.OK_ID);
        boolean ok = false;
        try {
            Integer.parseInt(pupIdText.getText());
            ok = true;
        } catch (RuntimeException e) {
            // nothing
        }
        b.setEnabled(ok);
    }

    protected void onBrowseEndpoint() {
        DirectoryDialog d = new DirectoryDialog(getShell(), SWT.NONE);
        d.setFilterPath(edexEndpointText.getText());
        String result = d.open();
        if (result != null)
            edexEndpointText.setText(result);
    }

	protected void onBrowseArchiveRoot() {
		DirectoryDialog d = new DirectoryDialog(getShell(), SWT.NONE);
        d.setFilterPath(archiveRootText.getText());
        String result = d.open();
        if (result != null)
        	archiveRootText.setText(result);
	}
	
    @Override
	protected void okPressed() {
		globals.collectionEnabled = collectionEnabledCheck.getSelection();
    	if (decompressProductsCheck != null)
    		globals.decompressProducts = decompressProductsCheck.getSelection();
    	if (edexEndpointText != null)
    		globals.edexEndpoint = edexEndpointText.getText();
		
		globals.pupID = Integer.parseInt(pupIdText.getText());
		globals.tdwrCollectionLimited = tdwrCollectionLimitedCheck
				.getSelection();
		globals.wmoSiteID = wmoSiteIdText.getText();

		if (globals.endpointConfig == null)
			globals.endpointConfig = new EndpointConfig();
		globals.endpointConfig.setArchiveRoot(archiveRootText.getText());
		globals.endpointConfig.setConnectionURL(edexJMSText.getText());
		globals.endpointConfig.setPrefixPathWithRadar(prefixWithRadarButton.getSelection());

		super.okPressed();
	}

    @Override
    public void create() {
        super.create();
        validate();
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Global Settings");
    }
}
