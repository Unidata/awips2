package com.raytheon.uf.viz.productbrowser.bookmarks;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

public class ProductBrowserBookmarksDialog extends CaveSWTDialog {

    protected ProductBrowserBookmarksDialog(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.INDEPENDENT_SHELL
                | CAVE.PERSPECTIVE_INDEPENDENT | CAVE.MODE_INDEPENDENT);
        setText("Product Bookmarks");
    }

    public static void createNewDialog() {
        ProductBrowserBookmarksDialog pbbd = new ProductBrowserBookmarksDialog(
                new Shell(Display.getDefault()));
        pbbd.open();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        final Tree tree = new Tree(shell, SWT.MULTI | SWT.BORDER);
        tree.setLayout(new GridLayout(1, true));
        tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        final Text text = new Text(shell, SWT.SINGLE | SWT.BORDER);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        text.setLayoutData(gridData);

        tree.addListener(SWT.MouseDoubleClick, new Listener() {
            public void handleEvent(Event e) {
                text.setText(tree.getSelection()[0].getText());
            }
        });

        Composite comp = new Composite(shell, SWT.NONE);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        comp.setLayout(new GridLayout(3, false));
        comp.setLayoutData(gridData);

        Button addButton = new Button(comp, SWT.PUSH);
        addButton.setText("Add Folder");
        addButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (!("").equals(text.getText())) {
                    TreeItem ti = null;
                    if (tree.getSelectionCount() != 0) {
                        tree.getSelection()[0].setExpanded(true);
                        ti = new TreeItem(tree.getSelection()[0], SWT.NONE);
                    } else {
                        ti = new TreeItem(tree, SWT.NONE);
                    }
                    ti.setText(text.getText());
                }
            }

        });

        Button removeButton = new Button(comp, SWT.PUSH);
        removeButton.setText("Remove");
        removeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (tree.getSelectionCount() != 0) {
                    tree.getSelection()[0].dispose();
                }
            }

        });

        Button renameButton = new Button(comp, SWT.PUSH);
        renameButton.setText("Rename");

        renameButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (!("").equals(text.getText())) {
                    if (tree.getSelectionCount() != 0) {
                        tree.getSelection()[0].setText(text.getText());
                    }
                }
            }
        });
    }
}