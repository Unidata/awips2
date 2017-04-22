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

import java.util.Arrays;

import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.config.LinkResource;
import com.raytheon.rcm.config.LinkType;
import com.raytheon.rcm.config.RadarConfig;

public class RadarConfigDialog extends Dialog {
    private RadarConfig rc;

    private WritableList linkResourceList;

    private TableViewer linkResourceViewer;
    
    private Button sendEnvDataCheckBox;

    private static final String[] columnTitles = { "Dedicated", "Host:Port",
            "Link Index", "Password", "Max RPS Size", "User Password",
            "Port Password" };

    private static final int[] columnWidths = { 8, 24, 7, 12, 14, 12, 9 };

    private class LRProvider implements ITableLabelProvider {

        public static final int DEDICATED = 0;

        public static final int LINK_ADDRESS = 1;

        public static final int LINK_INDEX = 2;

        public static final int TCM_PASSWD = 3;

        public static final int MAX_RPS_LIST_SIZE = 4;

        public static final int USER_PASSWORD = 5;

        public static final int PORT_PASSWORD = 6;

        public static final int N_PROPERTIES = 7;

        protected class LREditSupport extends EditingSupport {

            int columnIndex;

            CellEditor editor;

            public LREditSupport(ColumnViewer viewer, int columnIndex) {
                super(viewer);
                this.columnIndex = columnIndex;

                switch (columnIndex) {
                case DEDICATED:
                    editor = new CheckboxCellEditor((Composite) viewer
                            .getControl(), SWT.CHECK);
                    break;
                default:
                    editor = new TextCellEditor((Composite) viewer.getControl());
                    switch (columnIndex) {
                    case LINK_INDEX:
                    case MAX_RPS_LIST_SIZE:
                        editor.setValidator(new ICellEditorValidator() {
                            public String isValid(Object value) {
                                try {
                                    Integer.parseInt((String) value);
                                } catch (Exception e) {
                                    return "Must be a number.";
                                }

                                return null;
                            }
                        });
                        break;
                    }
                }
            }

            @Override
            protected boolean canEdit(Object element) {
                return canEditColumn(element, columnIndex);
            }

            @Override
            protected CellEditor getCellEditor(Object element) {
                return editor;
            }

            @Override
            protected Object getValue(Object element) {
                switch (columnIndex) {
                case DEDICATED:
                    return getV(element, columnIndex);
                default:
                    return getColumnText(element, columnIndex);
                }
            }

            @Override
            protected void setValue(Object element, Object value) {
                setV(element, columnIndex, value);
            }

        }

        protected boolean canEditColumn(Object element, int columnIndex) {
            LinkResource lr = (LinkResource) element;
            if (lr.isDedicated()) {
                return columnIndex != USER_PASSWORD
                        && columnIndex != PORT_PASSWORD;
            } else {
                return columnIndex != MAX_RPS_LIST_SIZE;
            }
        }

        protected Object getV(Object element, int columnIndex) {
            LinkResource lr = (LinkResource) element;
            switch (columnIndex) {
            case DEDICATED:
                return lr.isDedicated();
            case LINK_ADDRESS:
                return lr.getLinkAddress();
            case LINK_INDEX:
                return lr.getLinkIndex();
            case TCM_PASSWD:
                return lr.getTcmPassword();
            case MAX_RPS_LIST_SIZE:
                return lr.getMaxRpsListSize();
            case USER_PASSWORD:
                return lr.getUserPassword();
            case PORT_PASSWORD:
                return lr.getPortPassword();
            default:
                return null;
            }
        }

        private int toInteger(Object value) {
            if (value instanceof Number)
                return ((Number) value).intValue();
            else if (value != null)
                return Integer.parseInt((String) value);
            else
                return -1;
        }

        protected void setV(Object element, int columnIndex, Object value) {
            if (value == null) // Validation error? But errors are not popping
                               // up?
                return;

            LinkResource lr = (LinkResource) element;
            switch (columnIndex) {
            case DEDICATED:
                lr.setDedicated((Boolean) value);
                break;
            case LINK_ADDRESS:
                lr.setLinkAddress((String) value);
                break;
            case LINK_INDEX:
                lr.setLinkIndex(toInteger(value));
                break;
            case TCM_PASSWD:
                lr.setTcmPassword((String) value);
                break;
            case MAX_RPS_LIST_SIZE:
                lr.setMaxRpsListSize(toInteger(value));
                break;
            case USER_PASSWORD:
                lr.setUserPassword((String) value);
                break;
            case PORT_PASSWORD:
                lr.setPortPassword((String) value);
                break;
            }
            int i = linkResourceList.indexOf(lr);
            if (i != -1)
                linkResourceList.set(i, lr);
        }

        @Override
        public Image getColumnImage(Object element, int columnIndex) {
            return null;
        }

        @Override
        public String getColumnText(Object element, int columnIndex) {
            Object o = getV(element, columnIndex);
            if (o instanceof Boolean)
                return ((Boolean) o).booleanValue() ? "yes" : "no";
            else {
                if (canEditColumn(element, columnIndex))
                    return o != null ? o.toString() : "";
                else
                    return "----";
            }
        }

        @Override
        public void addListener(ILabelProviderListener listener) {
        }

        @Override
        public void dispose() {
        }

        @Override
        public boolean isLabelProperty(Object element, String property) {
            return false;
        }

        @Override
        public void removeListener(ILabelProviderListener listener) {
        }

        public EditingSupport getEditorSupport(ColumnViewer viewer,
                int columnIndex) {
            return new LREditSupport(viewer, columnIndex);
        }

    }

    private LRProvider lRProvider = new LRProvider();

    public RadarConfigDialog(Shell shell, RadarConfig rc) {
        super(shell);
        setShellStyle((getShellStyle() & ~SWT.APPLICATION_MODAL)
                | SWT.PRIMARY_MODAL);
        this.rc = rc;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite c = (Composite) super.createDialogArea(parent);
        GridData gd;
        GridLayout gl = (GridLayout) c.getLayout();
        gl.numColumns = 2;

        Label l = new Label(c, SWT.LEFT);
        l.setText("Links");

        Composite r = new Composite(c, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.pack = true;
        r.setLayout(rl);
        Button b;
        b = new Button(r, SWT.PUSH);
        b.setText("Add");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onAddLink();
            }
        });
        b = new Button(r, SWT.PUSH);
        b.setText("Remove");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onRemoveLinks();
            }
        });
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        r.setLayoutData(gd);

        linkResourceList = new WritableList();
        if (rc.getLinkResources() != null)
            linkResourceList.addAll(Arrays.asList(rc.getLinkResources()));

        TableViewer tv = new TableViewer(c, SWT.BORDER | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.MULTI | SWT.FULL_SELECTION);
        linkResourceViewer = tv;
        for (int i = 0; i < LRProvider.N_PROPERTIES; ++i) {
            TableViewerColumn column = new TableViewerColumn(tv, SWT.NONE);
            column.getColumn().setText(columnTitles[i]);
            column.getColumn().setWidth(
                    convertWidthInCharsToPixels(columnWidths[i]));
            column.getColumn().setResizable(true);
            column.setEditingSupport(lRProvider.getEditorSupport(tv, i));
        }
        tv.setLabelProvider(lRProvider);
        tv.setContentProvider(new ObservableListContentProvider());
        tv.setInput(linkResourceList);
        tv.getTable().setHeaderVisible(true);
        tv.getTable().setLinesVisible(true);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 2;
        gd.widthHint = convertWidthInCharsToPixels(86);
        gd.heightHint = convertHeightInCharsToPixels(8);
        tv.getControl().setLayoutData(gd);
        
        sendEnvDataCheckBox = new Button(c, SWT.CHECK);
        sendEnvDataCheckBox.setText("Send environmental data to this RPG");
        sendEnvDataCheckBox.setSelection(rc.isSendEnvironmentalData());
        if (! rc.isDedicated())
            sendEnvDataCheckBox.setVisible(false);

        return c;
    }

    protected void onRemoveLinks() {
        for (Object o : ((IStructuredSelection) linkResourceViewer
                .getSelection()).toList())
            linkResourceList.remove(o);
    }

    protected void onAddLink() {
        LinkResource lr = new LinkResource();
        lr.setLinkAddress("host:0");
        lr.setTcmPassword("passwd");
        lr.setLinkType(LinkType.TCP_WAN);
        linkResourceList.add(lr);
        linkResourceViewer.setSelection(new StructuredSelection(Arrays
                .asList(lr)));
    }

    @Override
    protected void okPressed() {
        try {
            rc.setLinkResources((LinkResource[]) linkResourceList
                    .toArray(new LinkResource[linkResourceList.size()]));
            rc.setSendEnvironmentalData(sendEnvDataCheckBox.getSelection());
        } catch (RuntimeException e) {
            return;
        }
        super.okPressed();
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("RPG Settings");
    }

}
