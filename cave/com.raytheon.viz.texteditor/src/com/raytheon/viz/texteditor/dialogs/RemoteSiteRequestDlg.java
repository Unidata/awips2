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

package com.raytheon.viz.texteditor.dialogs;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.GetWmoIdRequest;
import com.raytheon.uf.common.dataplugin.text.request.RemoteRetrievalRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.wmo.message.WMOHeader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.texteditor.command.CommandFactory;
import com.raytheon.viz.texteditor.command.CommandFailedException;
import com.raytheon.viz.texteditor.msgs.IWmoIdSelectionCallback;
import com.raytheon.viz.texteditor.util.TextEditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The Remote Site Request dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 9/13/07      368         lvenable    Initial creation.
 * 10/11/2007   482         grichard    Reformatted file.
 * 
 * </pre>
 * 
 * @author lvenable
 */
public class RemoteSiteRequestDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(RemoteSiteRequestDlg.class);

    /**
     * AFOS site ID text field.
     */
    private Text wsfoIdTF;

    /**
     * Product category text field.
     */
    private Text productCatTF;

    /**
     * Product designator text field.
     */
    private Text prodDesignatorTF;

    /**
     * Address text field. Defines the site where a text product or message is
     * sent.
     */
    private Text addresseeTF;
    
    private Button enterBtn;
    
    private String initialAfosID;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public RemoteSiteRequestDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.PERSPECTIVE_INDEPENDENT);
        setText("Send Request");
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        createInputFields();
        createBottomButtons();
        validate();
    }

    /**
     * Create the input fields.
     */
    private void createInputFields() {
        Composite topComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        gridLayout.horizontalSpacing = 20;
        topComp.setLayout(gridLayout);

        ModifyListener validator = new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                validate();                
            }
        };
        
        // Create the WSFO ID text field.
        GridData gd = new GridData(45, SWT.DEFAULT);
        wsfoIdTF = new Text(topComp, SWT.BORDER);
        wsfoIdTF.setTextLimit(3);
        wsfoIdTF.setLayoutData(gd);
        wsfoIdTF.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent event) {
                wsfoIdTF.setText(wsfoIdTF.getText().toUpperCase());
            }
        });

        // When the number of characters enter reaches the max limit and
        // the caret position is at the end then switch focus to another
        // text field.
        wsfoIdTF.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (wsfoIdTF.getCaretPosition() == wsfoIdTF.getTextLimit()) {
                    productCatTF.setFocus();
                }
            }
        });

        wsfoIdTF.addModifyListener(validator);

        Label wsfoIdLbl = new Label(topComp, SWT.NONE);
        wsfoIdLbl.setText("WSFO Identification");

        // Create the product category text field.
        gd = new GridData(45, SWT.DEFAULT);
        productCatTF = new Text(topComp, SWT.BORDER);
        productCatTF.setTextLimit(3);
        productCatTF.setLayoutData(gd);
        productCatTF.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent event) {
                productCatTF.setText(productCatTF.getText().toUpperCase());
            }
        });

        // When the number of characters enter reaches the max limit and
        // the caret position is at the end then switch focus to another
        // text field.
        productCatTF.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (productCatTF.getCaretPosition() == productCatTF
                        .getTextLimit()) {
                    prodDesignatorTF.setFocus();
                }
            }
        });

        productCatTF.addModifyListener(validator);
        
        Label productCatLbl = new Label(topComp, SWT.NONE);
        productCatLbl.setText("Product Category");

        // Create the product designator text field.
        gd = new GridData(45, SWT.DEFAULT);
        prodDesignatorTF = new Text(topComp, SWT.BORDER);
        prodDesignatorTF.setTextLimit(3);
        prodDesignatorTF.setLayoutData(gd);
        prodDesignatorTF.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent event) {
                prodDesignatorTF.setText(prodDesignatorTF.getText()
                        .toUpperCase());
            }
        });

        // When the number of characters enter reaches the max limit and
        // the caret position is at the end then switch focus to another
        // text field.
        prodDesignatorTF.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (prodDesignatorTF.getCaretPosition() == prodDesignatorTF
                        .getTextLimit()) {
                    addresseeTF.setFocus();
                }
            }
        });
        
        prodDesignatorTF.addModifyListener(validator);

        Label prodDesignatorLbl = new Label(topComp, SWT.NONE);
        prodDesignatorLbl.setText("Product Designator");

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        Label sepLbl = new Label(topComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // Create the addressee text field.
        gd = new GridData(45, SWT.DEFAULT);
        addresseeTF = new Text(topComp, SWT.BORDER);
        addresseeTF.setTextLimit(4);
        addresseeTF.setText("DEF");
        addresseeTF.setLayoutData(gd);
        addresseeTF.addFocusListener(new FocusAdapter() {
            public void focusLost(FocusEvent event) {
                addresseeTF.setText(addresseeTF.getText().toUpperCase());
            }
        });
        
        // When the number of characters enter reaches the max limit and
        // the caret position is at the end then switch focus to another
        // text field.
        addresseeTF.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (addresseeTF.getCaretPosition() == addresseeTF
                        .getTextLimit()) {
                    wsfoIdTF.setFocus();
                }
            }
        });

        addresseeTF.addModifyListener(validator);

        Label addresseeLbl = new Label(topComp, SWT.NONE);
        addresseeLbl.setText("Addressee");

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        sepLbl = new Label(topComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
        
        String ccc = "";
        String nnn = "";
        String xxx = "";
        try {
            ccc = initialAfosID.substring(0, 3);
            nnn = initialAfosID.substring(3, 6);
            xxx = initialAfosID.substring(6);
        } catch (Exception e) {
            // ignore
        }
        wsfoIdTF.setText(ccc);
        productCatTF.setText(nnn);
        prodDesignatorTF.setText(xxx);
    }

    /**
     * Create the bottom Enter and Cancel buttons.
     */
    private void createBottomButtons() {
        // Create a composite that will center added controls/composites.
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));
        buttonArea.setLayout(new GridLayout(1, false));

        // Create a composite to hold the enter and cancel buttons.
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayout(new GridLayout(2, true));

        // Create the Enter button.
        enterBtn = new Button(buttons, SWT.PUSH);
        enterBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        enterBtn.setText("Enter");
        enterBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (createReturnValue(false))
                    shell.dispose();
            }
        });

        // Create the Cancel button.
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(GridData.FILL_BOTH));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(null);
                shell.dispose();
            }
        });
    }
    
    private void validate() {
        if (enterBtn != null)
            enterBtn.setEnabled(createReturnValue(true));
    }

    // Copied from AWIPSHeaderBlockDlg.lookupWmoIDs()
    private boolean lookupWmoId(final RemoteRetrievalRequest rrRequest) {
        GetWmoIdRequest request = new GetWmoIdRequest();
        request.setAfosId(rrRequest.getAfosID());
        
        try {
            Object response = ThriftClient.sendRequest(request);
            if (response != null) {
                if (response instanceof AfosWmoIdDataContainer) {
                    AfosWmoIdDataContainer container = (AfosWmoIdDataContainer) response;
                    if (container.getErrorMessage() != null) {
                        statusHandler.handle(
                                        Priority.PROBLEM,
                                        "Error occurred looking up WMO IDs\nMessage from server["
                                                + container
                                                        .getErrorMessage()
                                                + "]");
                    }

                    List<AfosToAwips> list = container.getIdList();

                    if (list.size() > 1) {
                        ArrayList<String> ttaaiiIds = new ArrayList<String>(
                                list.size());
                        ArrayList<String> ccccIds = new ArrayList<String>(
                                list.size());
                        for (AfosToAwips id : list) {
                            ttaaiiIds.add(id.getWmottaaii());
                            ccccIds.add(id.getWmocccc());
                        }

                        
                        IWmoIdSelectionCallback cb = new IWmoIdSelectionCallback() {
                            @Override
                            public void setWmoId(String ttaaii, String cccc) {
                                rrRequest.setWmoHeader(ttaaii + ' ' + cccc);
                            }
                        };
                        WmoIdSelectionDialog dlg = new WmoIdSelectionDialog(
                                shell, cb, ttaaiiIds, ccccIds);
                        dlg.setBlockOnOpen(true);
                        dlg.open();
                        return rrRequest.getWmoHeader() != null &&
                            rrRequest.getWmoHeader().length() > 0;
                    } else if (list.size() == 1) {
                        AfosToAwips id = list.get(0);
                        rrRequest.setWmoHeader(id.getWmottaaii() + ' ' + id.getWmocccc());
                    } else {
                        rrRequest.setWmoHeader("");
                    }
                    
                    return true;
                } else {
                    statusHandler.handle(Priority.PROBLEM,
                            "Received unhandled WMO Id lookup response from server. Received obj of type ["
                                    + response.getClass() + "], contents["
                                    + response + "]");
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred looking up WMO IDs", e);
        }
        return false;
    }
    
    private Calendar createCalRelativeTo(Calendar relative, WMOHeader wmoHeader, int monthAdjustment) {
        Calendar c = new GregorianCalendar(relative.getTimeZone());
        c.setTimeInMillis(relative.getTimeInMillis());
        c.add(GregorianCalendar.MONTH, monthAdjustment);
        c.set(GregorianCalendar.DAY_OF_MONTH, wmoHeader.getDay());
        c.set(GregorianCalendar.HOUR_OF_DAY, wmoHeader.getHour());
        c.set(GregorianCalendar.MINUTE, wmoHeader.getMinute());
        c.set(GregorianCalendar.SECOND, 0);
        c.set(GregorianCalendar.MILLISECOND, 0);
        return c;
    }
    
    private Calendar getCloserCalendar(Calendar reference, Calendar a, Calendar b) {
        return Math.abs(a.getTimeInMillis() - reference.getTimeInMillis()) <
            Math.abs(b.getTimeInMillis() - reference.getTimeInMillis()) ?
                    a : b;
    }

    private boolean createReturnValue(boolean validateOnly) {
        String ccc = wsfoIdTF.getText();
        String nnn = productCatTF.getText();
        String xxx = prodDesignatorTF.getText();
        String addr = addresseeTF.getText();
        if (ccc.length() != 3 || nnn.length() != 3 ||
                xxx.length() < 1 || xxx.length() > 3 ||
                addr.length() < 1)
            return false;
        
        if (validateOnly)
            return true;
        
        String afosID = ccc + nnn + xxx;
        GetWmoIdRequest request = new GetWmoIdRequest();
        request.setAfosId(afosID);
        
        RemoteRetrievalRequest req = new RemoteRetrievalRequest();
        // TODO: Translate addr via awipsSites.txt or siteDistLists.txt
        req.setAddressee(addr);
        req.setAfosID(afosID);
        
        List<StdTextProduct> latest = null;
        try {
            latest = CommandFactory.getAfosCommand(req.getAfosID()).executeCommand(TextEditorUtil
                    .getTextDbsrvTransport());
        } catch (CommandFailedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving metatdata", e);
            // but keep going...
        }
        
        if (latest != null && latest.size() > 0) {
            Calendar c = new GregorianCalendar(TimeZone
                    .getTimeZone("GMT"));
            c.setTimeInMillis(latest.get(0).getRefTime());
            req.setMostRecentTime(c.getTimeInMillis()); // default
            try {
                WMOHeader wmo = new WMOHeader(latest.get(0).getProduct().getBytes());
                Calendar t = getCloserCalendar(c, createCalRelativeTo(c, wmo, 0), 
                        createCalRelativeTo(c, wmo, 1));
                t = getCloserCalendar(c, t, createCalRelativeTo(c, wmo, -1));
                req.setMostRecentTime(t.getTimeInMillis());
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error determining product time", e);
            }
        } else
            req.setMostRecentTime(0);

        if (! lookupWmoId(req))
            return false;
        
        req.setValidTime(System.currentTimeMillis() + 600 * 1000); // Current time plus 10 minutes
        
        setReturnValue(req);
        return true;
    }

    public void setRequest(RemoteRetrievalRequest lastRemoteRetrievalRequest) {
        initialAfosID = null;
        if (lastRemoteRetrievalRequest != null)
            initialAfosID = lastRemoteRetrievalRequest.getAfosID();
    }
}
