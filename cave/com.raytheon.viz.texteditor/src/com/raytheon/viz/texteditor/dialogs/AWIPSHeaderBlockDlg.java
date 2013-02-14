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

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.db.AfosToAwips;
import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.dataplugin.text.request.GetAfosIdRequest;
import com.raytheon.uf.common.dataplugin.text.request.GetWmoIdRequest;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.texteditor.AfosBrowserModel;
import com.raytheon.viz.texteditor.msgs.IAfosIdSelectionCallback;
import com.raytheon.viz.texteditor.msgs.IWmoIdSelectionCallback;
import com.raytheon.viz.texteditor.util.TextEditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Dialog that allows the user to edit the AWIPS header block.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 9/13/07      368         lvenable    Initial creation.
 * 10/11/2007   482         grichard    Reformatted file.
 * 10/18/2007   482         grichard    Implemented build 9 features.
 * 12/07/2007   582         grichard    Implemented build 12 features.
 * 1/10/2008    722         grichard    Implemented localization.
 * 1/5/2009     1688        grichard    Restrict BBB ID.
 * 3/23/2009    1956        bwoodle     Change SiteNode and WMO ID retrieval for hazard prods.
 * 7/29/2009    2718        rjpeter     Updated interactions between dialogs.
 * Aug 28, 2009 2924        rjpeter     Updated to automate filling out of other fields based on input.
 * 04/14/2010   4734        mhuang      Corrected StdTextProduct import 
 *                                       dependency
 * 06/28/2010   4639        cjeanbap    Allow user to create a new text product.
 * 
 * 01/26/2012   14468       D.Friedman  Fix initial BBB field selection.
 * 05/30/2012   15046       D.Friedman  Always set addressee field to ALL.
 * 06/19/2012   14975       D.Friedman  Run callback when dialog is dismissed.
 * 07/26/2012   15171       rferrel     Disable editor's send and clear AFOS PIL fields when
 *                                      invalid product Id and user want to edit it anyway.
 * 09/20/2012   1196        rferrel     Changing dialogs being called to not block.
 * 11/26/2012	14526		mgamazaychikov	Added traverse listener for RETURN key
 * </pre>
 * 
 * @author lvenable
 */
public class AWIPSHeaderBlockDlg extends CaveSWTDialog implements
        IAfosIdSelectionCallback, IWmoIdSelectionCallback {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AWIPSHeaderBlockDlg.class);

    /**
     * WMO data type and area indicator text field.
     */
    private StyledText wmoTtaaiiTF;

    /**
     * International location indicator text field.
     */
    private StyledText ccccTF;

    /**
     * Combo box Type of message/indicator group.
     */
    private Combo bbbCboBx;

    /**
     * Combo box Type of message/indicator group version.
     */
    private Combo bbbVerCboBx;

    /**
     * AFOS site ID text field.
     */
    private StyledText wsfoIdTF;

    /**
     * Product category text field.
     */
    private StyledText prodCatTF;

    /**
     * Product designator text field.
     */
    private StyledText prodDesignatorTF;

    /**
     * Address text field. Defines the site where a text product or message is
     * sent.
     */
    private StyledText addresseeTF;

    /**
     * Zeros button. Puts "000" into the addressee text field. Zeros indicates
     * the product is to be stored in the database but not distributed.
     */
    private Button zerosBtn;

    /**
     * DEF button. Puts "DEF" into the addressee text field. DEF indicates
     * products are sent to a predefined group of sites.
     */
    private Button defBtn;

    /**
     * ALL button. Puts "ALL" into the addressee text field. ALL indicates
     * products are sent to all sites.
     */
    private Button allBtn;

    /**
     * ENTER button. Transitions the display from AWIPS Header entry into Text
     * Editing.
     */
    private Button enterBtn;

    /**
     * List of characters in the BBB combo box.
     */
    private String[] BBB_LIST = { "RRx", "CCx", "AAx", "NOR" };

    /**
     * List of characters in the BBB version combo box.
     */
    private String[] CHAR_LIST = { "A", "B", "C", "D", "E", "F", "G", "H", "I",
            "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
            "W", "X" };

    /**
     * Interface variable for AWIPS Header callback.
     */
    private TextEditorDialog parentEditor = null;

    private boolean lookupAllowed = true;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public AWIPSHeaderBlockDlg(Shell parent, TextEditorDialog cbClient) {
        super(parent, SWT.DIALOG_TRIM | SWT.PRIMARY_MODAL,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        this.setText("AWIPS Header Block");
        this.parentEditor = cbClient;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (parentEditor != null)
                    parentEditor.headerBlockDlgDismissed(Boolean.TRUE
                            .equals(getReturnValue()));
            }
        });

        setReturnValue(false);

        createWmoIdFields();
        createAfosIdFields();
        createAddresseeFields();
        createBottomButtons();
        setupTextFieldListeners();

        // this was after shell.pack() before, any reason why?
        checkEnableEnter();
        /*
         * 14526 - Add the traverse listener for RETURN key 
         */
        setTraverseListenerReturn();
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData();
    }

    /**
     * Create all of the WMO ID controls.
     */
    private void createWmoIdFields() {
        // Create the composite to contain the WMO ID controls.
        Composite wmoIdComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        wmoIdComp.setLayout(gridLayout);

        // Create the WMO ID text field.
        GridData gd = new GridData(55, SWT.DEFAULT);
        wmoTtaaiiTF = new StyledText(wmoIdComp, SWT.BORDER);
        wmoTtaaiiTF.setTextLimit(6);
        wmoTtaaiiTF.setLayoutData(gd);

        StdTextProduct textProd = parentEditor.getStdTextProduct();

        // Display the current/default WMO ID in the AWIPS Header Block and set
        // it in the text display editor as well.
        if (textProd != null && textProd.getWmoid() != null) {
            wmoTtaaiiTF.setText(textProd.getWmoid());
        }

        Label ttaaiiLbl = new Label(wmoIdComp, SWT.NONE);
        ttaaiiLbl.setText("TTAAii");

        // Create the international location indicator text field.
        gd = new GridData(45, SWT.DEFAULT);
        ccccTF = new StyledText(wmoIdComp, SWT.BORDER);
        ccccTF.setTextLimit(4);
        ccccTF.setLayoutData(gd);
        if (textProd != null && textProd.getSite() != null) {
            ccccTF.setText(textProd.getSite());
        }

        Label ccccLbl = new Label(wmoIdComp, SWT.NONE);
        ccccLbl.setText("CCCC");

        // Create the message/indicator group combo box.
        gd = new GridData(70, SWT.DEFAULT);
        bbbCboBx = new Combo(wmoIdComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        bbbCboBx.setItems(BBB_LIST);
        bbbCboBx.select(3);
        bbbCboBx.setLayoutData(gd);

        bbbCboBx.addFocusListener(new FocusListener() {

            @Override
            public void focusLost(FocusEvent e) {
                if (bbbCboBx.getSelectionIndex() == -1) {
                    bbbCboBx.select(BBB_LIST.length - 1);
                }
            }

            @Override
            public void focusGained(FocusEvent e) {
            }
        });

        Label bbbLbl = new Label(wmoIdComp, SWT.NONE);
        bbbLbl.setText("BBB");

        // Create the message/indicator version group combo box.
        gd = new GridData(70, SWT.DEFAULT);
        bbbVerCboBx = new Combo(wmoIdComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        bbbVerCboBx.setItems(CHAR_LIST);
        bbbVerCboBx.select(0);
        bbbVerCboBx.setLayoutData(gd);

        bbbVerCboBx.addFocusListener(new FocusListener() {

            @Override
            public void focusLost(FocusEvent e) {
                if (bbbVerCboBx.getSelectionIndex() == -1) {
                    bbbVerCboBx.select(0);
                }
            }

            @Override
            public void focusGained(FocusEvent e) {
            }
        });

        Label bbbVerLbl = new Label(wmoIdComp, SWT.NONE);
        bbbVerLbl.setText("BBB Version");
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        // Display the current/default BBB ID in the AWIPS Header Block and set
        // it in the text display editor as well.
        String bbb = null;
        if (textProd != null) {
            bbb = textProd.getBbbid();
        }
        if (bbb == null || bbb.length() == 0) {
            bbbVerCboBx.setEnabled(false);
            parentEditor.setCurrentBbbId("NOR");
        } else {
            String c0 = new Character(bbb.charAt(0)).toString();
            if ((!c0.equals("N")) && (!c0.equals("")) && (!c0.equals(" "))) {
                String c1 = new Character(bbb.charAt(1)).toString();
                String c2 = new Character(bbb.charAt(2)).toString();
                String c3 = new Character('x').toString();
                bbbCboBx.setText(c0 + c1 + c3);
                bbbVerCboBx.setText(c2);
            } else {
                bbbVerCboBx.setEnabled(false);
            }
        }

        // Use this listener to adjust the version combo box. When the enter
        // editor button is pressed.
        bbbCboBx.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (bbbCboBx.getSelectionIndex() == -1) {
                    bbbVerCboBx.setEnabled(false);
                } else if (!((new Character(bbbCboBx.getItem(
                        bbbCboBx.getSelectionIndex()).charAt(0)).toString())
                        .equals("N"))) {
                    bbbVerCboBx.setEnabled(true);
                } else {
                    bbbVerCboBx.setEnabled(false);
                }
            }
        });
    }

    /**
     * Create the AFOS ID fields.
     */
    private void createAfosIdFields() {
        // Create the composite to contain the WMO ID controls.
        Composite afosIdComp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginLeft = 1;
        gridLayout.marginTop = 1;
        gridLayout.horizontalSpacing = 20;
        afosIdComp.setLayout(gridLayout);

        // Create the WSFO ID text field.
        GridData gd = new GridData(45, SWT.DEFAULT);
        wsfoIdTF = new StyledText(afosIdComp, SWT.BORDER);
        wsfoIdTF.setTextLimit(3);
        wsfoIdTF.setLayoutData(gd);

        Label wsfoIdLbl = new Label(afosIdComp, SWT.NONE);
        wsfoIdLbl.setText("WSFO ID (Optional)");

        // Create the Product category text field.
        gd = new GridData(45, SWT.DEFAULT);
        prodCatTF = new StyledText(afosIdComp, SWT.BORDER);
        prodCatTF.setTextLimit(3);
        prodCatTF.setLayoutData(gd);

        Label prodCatLbl = new Label(afosIdComp, SWT.NONE);
        prodCatLbl.setText("Product Category");

        // Create the product designator text field.
        gd = new GridData(45, SWT.DEFAULT);
        prodDesignatorTF = new StyledText(afosIdComp, SWT.BORDER);
        prodDesignatorTF.setTextLimit(3);
        prodDesignatorTF.setLayoutData(gd);
        prodDesignatorTF.addFocusListener(new FocusListener() {

            @Override
            public void focusLost(FocusEvent e) {
                StringBuilder sb = new StringBuilder(prodDesignatorTF.getText()
                        .trim());
                if (sb.length() > 0) {
                    // Pad field with trailing spaces.
                    while (sb.length() < 3) {
                        sb.append(' ');
                    }

                    // Only trigger the modification listener when there is a
                    // real change.
                    String value = sb.toString();
                    if (!value.equals(prodDesignatorTF.getText())) {
                        prodDesignatorTF.setText(value);
                    }
                }
            }

            @Override
            public void focusGained(FocusEvent e) {
            }
        });

        Label prodDesignatorLbl = new Label(afosIdComp, SWT.NONE);
        prodDesignatorLbl.setText("Product Designator");

        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        // Display the current/default Product Node in the AWIPS Header Block
        // and set it in the text display editor as well.
        StdTextProduct textProd = parentEditor.getStdTextProduct();
        if (textProd != null) {
            if (textProd.getSite() != null) {
                String CCCcode = SiteMap.getInstance().getCCCFromXXXCode(
                        textProd.getSite());
                if (null == CCCcode && textProd.getCccid() != null) {
                    CCCcode = SiteMap.getInstance().getCCCFromXXXCode(
                            textProd.getCccid());
                }
                if (null == CCCcode) {
                    CCCcode = "";
                }
                wsfoIdTF.setText(CCCcode);
            } else {
                wsfoIdTF.setText(textProd.getCccid());
            }
            prodCatTF.setText(textProd.getNnnid());
            prodDesignatorTF.setText(textProd.getXxxid());
        }
    }

    /**
     * Create the addressee control fields.
     */
    private void createAddresseeFields() {
        // Create the composite to contain the addressee controls.
        Composite addresseeComp = new Composite(shell, SWT.NONE);
        RowLayout rowLayout = new RowLayout();
        rowLayout.marginLeft = 5;
        rowLayout.spacing = 1;
        rowLayout.marginTop = 1;
        addresseeComp.setLayout(rowLayout);

        RowData rd = new RowData(45, SWT.DEFAULT);
        addresseeTF = new StyledText(addresseeComp, SWT.BORDER);
        addresseeTF.setTextLimit(4);
        addresseeTF.setLayoutData(rd);
        // Set the "default" addressee to "ALL".
        addresseeTF.setText("ALL");

        // When the number of characters enter reaches the max limit and
        // the caret position is at the end then switch focus to the next
        // text field.
        addresseeTF.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (addresseeTF.getCaretOffset() == addresseeTF.getTextLimit()) {
                    wmoTtaaiiTF.setFocus();
                }

                handleAddresseeModified();
            }
        });

        createVerticallyCenteredLabel(addresseeComp, " Addressee  ");

        // Create the zeros button.
        zerosBtn = new Button(addresseeComp, SWT.TOGGLE);
        zerosBtn.setText("000");
        zerosBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (zerosBtn.getSelection() == false) {
                    return;
                }

                defBtn.setSelection(false);
                allBtn.setSelection(false);
                addresseeTF.setText("000");
            }
        });

        // Create the "DEF" (predefined) button.
        defBtn = new Button(addresseeComp, SWT.TOGGLE);
        defBtn.setText("DEF");
        defBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (defBtn.getSelection() == false) {
                    return;
                }

                zerosBtn.setSelection(false);
                allBtn.setSelection(false);
                addresseeTF.setText("DEF");
            }
        });

        // Create the all button.
        allBtn = new Button(addresseeComp, SWT.TOGGLE);
        allBtn.setText("ALL");
        allBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (allBtn.getSelection() == false) {
                    return;
                }

                zerosBtn.setSelection(false);
                defBtn.setSelection(false);
                addresseeTF.setText("ALL");
            }
        });
        handleAddresseeModified();

        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
    }

    private void setupTextFieldListeners() {
        // forces all fields to uppercase and only allows numbers/digits
        textFieldVerifyListener(wmoTtaaiiTF);
        textFieldVerifyListener(ccccTF);
        textFieldVerifyListener(wsfoIdTF);
        textFieldVerifyListener(prodCatTF);
        textFieldVerifyListener(prodDesignatorTF);
        textFieldVerifyListener(addresseeTF);

        // forces overwrite and arrow key traversal
        textFieldKeyListener(wmoTtaaiiTF, addresseeTF, ccccTF);
        textFieldKeyListener(ccccTF, wmoTtaaiiTF, wsfoIdTF);
        textFieldKeyListener(wsfoIdTF, ccccTF, prodCatTF);
        textFieldKeyListener(prodCatTF, wsfoIdTF, prodDesignatorTF);
        textFieldKeyListener(prodDesignatorTF, prodCatTF, addresseeTF);
        textFieldKeyListener(addresseeTF, prodDesignatorTF, wmoTtaaiiTF);

        textFieldModifyListener(wmoTtaaiiTF, ccccTF, true);
        textFieldModifyListener(ccccTF, wsfoIdTF, true);
        textFieldModifyListener(wsfoIdTF, prodCatTF, false);
        textFieldModifyListener(prodCatTF, prodDesignatorTF, false);

        // special handling due to checks for 000 as well as lookup called with
        // any number of characters as this field can be less than max length
        prodDesignatorTF.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (prodDesignatorTF.getText().equals("000")) {
                    TextEditorUtil.userInformation(shell,
                            "Product Designator cannot be 000");
                } else {
                    lookupWmoIDs();

                    if (!isDisposed()) {
                        if (prodDesignatorTF.getCaretOffset() == prodDesignatorTF
                                .getTextLimit()) {
                            addresseeTF.setFocus();
                        }
                    }
                }

                if (!isDisposed()) {
                    checkEnableEnter();
                }
            }
        });
    }

    /**
     * Create the bottom control buttons.
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
        enterBtn.setLayoutData(new GridData(80, SWT.DEFAULT));
        enterBtn.setText("Enter");
        // enterBtn.setEnabled(false);
        enterBtn.setEnabled(true);
        enterBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	enterBtnPressed();
            }

        });

        // Create the Cancel button.
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setLayoutData(new GridData(80, SWT.DEFAULT));
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                shell.dispose();
            }
        });

    }

	protected void enterBtnPressed() {
		/*
		 * 14526 - Added the check for when RETURN key is pressed
		 * but the "Enter" button is not enabled.
		 */
		if ( !enterBtn.getEnabled()) {
			return;
		}
		boolean sendEnabled = true;
		if (!isProductValid()) {
			// Notify the user that the product may not be valid.
			//
			// TODO cannot use a model MessageBox here. If displayed
			// when an Alarm Alert Bell appears Cave freezes and
			// nothing can be done. Need to change this to extend
			// CaveSWTDialog in a similar manner to
			// WarnGenConfirmationDlg. Better solution if possible
			// change AlermAlertBell so modal MessagBox can be used..
			MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES
					| SWT.NO);
			mb.setMessage("Product Designator " + wsfoIdTF.getText()
					+ prodCatTF.getText() + prodDesignatorTF.getText()
					+ " is not in the list of valid products. Use it anyway?");
			if (mb.open() == SWT.NO) {
				return;
			}
			parentEditor.enableSend(false);
			sendEnabled = false;
		} else {
			parentEditor.enableSend(true);
		}

		// call the set methods
		parentEditor.setCurrentWmoId(wmoTtaaiiTF.getText());
		parentEditor.setCurrentSiteId(ccccTF.getText());
		if (sendEnabled) {
			parentEditor.setCurrentWsfoId(wsfoIdTF.getText());
			parentEditor.setCurrentProdCategory(prodCatTF.getText());
			parentEditor.setCurrentProdDesignator(prodDesignatorTF.getText());
		} else {
			parentEditor.setCurrentWsfoId("");
			parentEditor.setCurrentProdCategory("");
			parentEditor.setCurrentProdDesignator("");
		}
		parentEditor.setAddressee(addresseeTF.getText());
		setBbbId();
		setReturnValue(true);
		shell.dispose();
	}
		

	/**
     * This is a convenience method that will center a label in a RowLayout.
     * When controls are placed in a RowLayout they are "aligned" at the top of
     * the cell.
     * 
     * @param parentComp
     * @param labelText
     */
    private void createVerticallyCenteredLabel(Composite parentComp,
            String labelText) {
        Composite vertTextRowComp = new Composite(parentComp, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        vertTextRowComp.setLayout(gridLayout);

        GridData gd = new GridData(GridData.VERTICAL_ALIGN_CENTER);
        Label tempLbl = new Label(vertTextRowComp, SWT.NONE);
        tempLbl.setText(labelText);
        tempLbl.setLayoutData(gd);
    }

    /**
     * Method to set the BBB ID
     */
    private void setBbbId() {
        String bbb = bbbCboBx.getText();
        String bbbVer = bbbVerCboBx.getText();
        if (bbb != null) {
            if (bbb.equals("NOR")) {
                // Set the text in bbbId from bbb[0] + bbb[1] + bbbVer
                parentEditor.setCurrentBbbId(bbb);
            } else if (bbb.length() > 0 && bbbVer.length() > 0) {
                // Set the text in bbbId from bbb[0] + bbb[1] + bbbVer
                parentEditor.setCurrentBbbId(bbb.substring(0, 2) + bbbVer);
            }
        }
    }

    /**
     * Method to determine whether a product is valid
     * 
     * @return indicator
     */
    private boolean isProductValid() {
        boolean result = false;

        String nnn = prodCatTF.getText();

        if (nnn.equals("WRK")) {
            result = true;
        } else {
            String ccc = wsfoIdTF.getText();

            if (ccc != null && ccc.length() > 0) {
                result = AfosBrowserModel.getInstance().contains(ccc, nnn,
                        prodDesignatorTF.getText().trim());
            } else {
                result = AfosBrowserModel.getInstance().contains(nnn,
                        prodDesignatorTF.getText().trim());
            }
        }
        return result;
    }

    private void lookupAfosIDs() {
        if (lookupAllowed
                && wmoTtaaiiTF.getText().length() == wmoTtaaiiTF.getTextLimit()
                && ccccTF.getText().length() == ccccTF.getTextLimit()) {
            GetAfosIdRequest request = new GetAfosIdRequest();
            request.setTtaaii(wmoTtaaiiTF.getText());
            request.setCccc(ccccTF.getText());
            lookupAllowed = false;

            try {
                Object response = ThriftClient.sendRequest(request);
                if (response != null) {
                    if (response instanceof AfosWmoIdDataContainer) {
                        AfosWmoIdDataContainer container = (AfosWmoIdDataContainer) response;
                        if (container.getErrorMessage() != null) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Error occurred looking up AFOS IDs\nMessage from server["
                                                    + container
                                                            .getErrorMessage()
                                                    + "]");
                        }

                        java.util.List<AfosToAwips> list = container
                                .getIdList();

                        if (list.size() > 1) {
                            ArrayList<String> afosIds = new ArrayList<String>(
                                    list.size());
                            for (AfosToAwips id : list) {
                                afosIds.add(id.getAfosid());
                            }

                            AfosIdSelectionDialog dlg = new AfosIdSelectionDialog(
                                    shell, this, afosIds);
                            dlg.setCloseCallback(new ICloseCallback() {

                                @Override
                                public void dialogClosed(Object returnValue) {
                                    lookupAllowed = true;
                                }
                            });
                            dlg.setBlockOnOpen(false);
                            dlg.open();
                            return;
                        } else if (list.size() == 1) {
                            setAfosId(list.get(0).getAfosid());
                        } else {
                            setAfosId("");
                        }
                    } else {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Received unhandled AFOS Id lookup response from server. Received obj of type ["
                                                + response.getClass()
                                                + "], contents["
                                                + response
                                                + "]");
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occurred looking up AFOS IDs", e);
            }

            lookupAllowed = true;
        }
    }

    private void lookupWmoIDs() {
        if (lookupAllowed
                && wsfoIdTF.getText().length() == wsfoIdTF.getTextLimit()
                && prodCatTF.getText().length() == prodCatTF.getTextLimit()
                && prodDesignatorTF.getText().length() > 0) {
            GetWmoIdRequest request = new GetWmoIdRequest();
            request.setAfosId(wsfoIdTF.getText() + prodCatTF.getText()
                    + prodDesignatorTF.getText());
            lookupAllowed = false;

            try {
                Object response = ThriftClient.sendRequest(request);
                if (response != null) {
                    if (response instanceof AfosWmoIdDataContainer) {
                        AfosWmoIdDataContainer container = (AfosWmoIdDataContainer) response;
                        if (container.getErrorMessage() != null) {
                            statusHandler
                                    .handle(Priority.PROBLEM,
                                            "Error occurred looking up WMO IDs\nMessage from server["
                                                    + container
                                                            .getErrorMessage()
                                                    + "]");
                        }

                        java.util.List<AfosToAwips> list = container
                                .getIdList();

                        if (list.size() > 1) {
                            ArrayList<String> ttaaiiIds = new ArrayList<String>(
                                    list.size());
                            ArrayList<String> ccccIds = new ArrayList<String>(
                                    list.size());
                            for (AfosToAwips id : list) {
                                ttaaiiIds.add(id.getWmottaaii());
                                ccccIds.add(id.getWmocccc());
                            }

                            WmoIdSelectionDialog dlg = new WmoIdSelectionDialog(
                                    shell, this, ttaaiiIds, ccccIds);
                            dlg.setBlockOnOpen(false);
                            dlg.setCloseCallback(new ICloseCallback() {

                                @Override
                                public void dialogClosed(Object returnValue) {
                                    lookupAllowed = true;
                                }
                            });
                            dlg.open();
                            return;
                        } else if (list.size() == 1) {
                            AfosToAwips id = list.get(0);
                            setWmoId(id.getWmottaaii(), id.getWmocccc());
                        } else {
                            setWmoId("", "");
                        }
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
            lookupAllowed = true;
        }
    }

    @Override
    public void setAfosId(String afosId) {
        wsfoIdTF.setText("");
        prodCatTF.setText("");
        prodDesignatorTF.setText("");

        if (afosId != null && afosId.length() > 0) {
            if (afosId.length() > 3) {
                wsfoIdTF.setText(afosId.substring(0, 3));

                if (afosId.length() > 6) {
                    prodCatTF.setText(afosId.substring(3, 6));

                    if (afosId.length() > 9) {
                        prodDesignatorTF.setText(afosId.substring(6, 9));
                    } else {
                        prodDesignatorTF.setText(afosId.substring(6,
                                afosId.length()));
                    }
                } else {
                    prodCatTF.setText(afosId.substring(3, afosId.length()));
                }
            } else {
                wsfoIdTF.setText(afosId.substring(0, afosId.length()));
            }
        }
    }

    @Override
    public void setWmoId(String ttaaii, String cccc) {
        wmoTtaaiiTF.setText(ttaaii);
        ccccTF.setText(cccc);
    }

    private void checkEnableEnter() {
        boolean enable = false;

        if (prodCatTF.getCharCount() == prodCatTF.getTextLimit()
                && prodDesignatorTF.getCharCount() > 0
                && !prodDesignatorTF.getText().equals("000")) {
            enable = true;
        }
        enterBtn.setEnabled(enable);
    }

    private void textFieldKeyListener(final StyledText tf,
            final StyledText previousTF, final StyledText nextTF) {
        tf.addKeyListener(new KeyAdapter() {

            @Override
            public void keyPressed(KeyEvent e) {
                char c = e.character;

                if (Character.isLetterOrDigit(c) || Character.isSpaceChar(c)) {
                    int pos = tf.getCaretOffset();
                    String text = tf.getText();

                    if (text.length() > pos) {
                        StringBuilder b = new StringBuilder(text);
                        b.deleteCharAt(pos);
                        tf.setText(b.toString());
                        tf.setSelection(pos);
                    }
                } else if (e.keyCode == SWT.ARROW_UP) {
                    previousTF.setFocus();
                    previousTF.selectAll();
                } else if (e.keyCode == SWT.ARROW_DOWN) {
                    nextTF.setFocus();
                    nextTF.selectAll();
                }
            }
        });
        
        /*
         * 14526 - Add the traverse listener for RETURN key 
         */
        tf.addTraverseListener(new TraverseListener() {
            @Override
            public void keyTraversed(TraverseEvent te) {
                te.doit = true;
            }
        });
        
    }

    private void textFieldVerifyListener(final StyledText tf) {
        tf.addVerifyListener(new VerifyListener() {
            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
                StringBuilder b = null;
                int posMod = 0;
                char[] chars = e.text.toCharArray();
                for (int i = 0; i < chars.length; i++) {
                    char c = chars[i];
                    if (!Character.isLetterOrDigit(c)
                            && !Character.isSpaceChar(c)) {
                        if (b == null) {
                            b = new StringBuilder(e.text);
                        }
                        b.deleteCharAt(i - posMod++);
                    }
                }

                if (b != null) {
                    e.text = b.toString();
                }
            }
        });
    }

    private void textFieldModifyListener(final StyledText tf,
            final StyledText nextTF, final boolean callAfosLookup) {
        tf.addModifyListener(new ModifyListener() {
            public void modifyText(ModifyEvent event) {
                if (tf.getCharCount() == tf.getTextLimit()) {
                    if (callAfosLookup) {
                        lookupAfosIDs();
                    } else {
                        lookupWmoIDs();
                    }
                }

                if (!isDisposed()) {
                    if (tf.getCaretOffset() == tf.getTextLimit()) {
                        nextTF.setFocus();
                        nextTF.selectAll();
                    }

                    checkEnableEnter();
                }
            }
        });
    }

    private void handleAddresseeModified() {
        // If the user changes the text in the addressee text field
        // then update the toggle buttons.
        String addressee = addresseeTF.getText();
        if (zerosBtn != null)
            zerosBtn.setSelection("000".equals(addressee));
        if (defBtn != null)
            defBtn.setSelection("DEF".equals(addressee));
        if (allBtn != null)
            allBtn.setSelection("ALL".equals(addressee));
    }
    
    /*
     * Adds the traverse listener for RETURN key 
     */
	private void setTraverseListenerReturn() {
		shell.addTraverseListener(new TraverseListener() {
            @Override
            public void keyTraversed(TraverseEvent event) {
                if (event.detail == SWT.TRAVERSE_RETURN) {
                	enterBtnPressed();
                }
            }

        });
	}
}
