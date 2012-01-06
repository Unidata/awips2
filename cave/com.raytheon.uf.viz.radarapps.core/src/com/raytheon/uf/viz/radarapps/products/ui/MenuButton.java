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
package com.raytheon.uf.viz.radarapps.products.ui;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;

public class MenuButton extends MinimumSizeComposite implements SelectionListener, DisposeListener {
	private Button button;

	public MenuButton(Composite parent) {
		super(parent, SWT.NONE);
		setLayout(new FillLayout(SWT.HORIZONTAL));
		button = new Button(this, SWT.PUSH);
		/*// This produced annoying behavoir... 
		button.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDown(MouseEvent e) {
				if (e.button == 1) {
					doPopup();
				}
			}

		});
		*/
		button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				doPopup();
			}
		});
		addDisposeListener(this);
		button.setText("Xxxx Xxxxx Xxxx Xxxxxxxxx (XXX)");
		setMinimumSize(button.computeSize(SWT.DEFAULT, SWT.DEFAULT));
	}
	
	public void setEnabled(boolean enabled) {
		button.setEnabled(enabled);
	}

	private void doPopup() {
		boolean isEnabled = button.isEnabled();
		if (! isEnabled)
			return;
		/*
		button.setEnabled(false);
		button.setEnabled(true);
		*/
		Menu menu = getMenu();
		if (menu == null)
			return;
		Point sz = getSize();
		Point p = toDisplay(0, sz.y);
		menu.setLocation(p);
		menu.setVisible(true);
	}

	@Override
	public void setMenu(Menu menu) {
		//Menu oldMenu = getMenu();
		/*
		if (oldMenu != null)
			oldMenu.removeMenuListener(this);
		*/
		super.setMenu(menu);
		realizeMenu();
	}
	
	public void updateMenu() {
		realizeMenu();
	}

	private ArrayList<MenuItem> menuItems = new ArrayList<MenuItem>();
	private void realizeMenu() {
		unrealizeMenu();		
		realizeMenu1(getMenu());
	}

	private void realizeMenu1(Menu menu) {
		if (menu != null) {
			for (MenuItem mi : menu.getItems()) {
				if ((mi.getStyle() & SWT.CASCADE) == 0)
					mi.addSelectionListener(this);
				else
					realizeMenu1(mi.getMenu());
			}
		}
	}

	@Override
	public void widgetDefaultSelected(SelectionEvent e) {
		widgetSelected(e);
	}

	private ArrayList<SelectionListener> listeners =
		new ArrayList<SelectionListener>();
	private MenuItem selectedItem;
	@Override
	public void widgetSelected(SelectionEvent e) {
		MenuItem mi;
		try {
			mi = (MenuItem) e.widget;
		} catch (ClassCastException exc) {
			return;
		}
		setSelectedItem(mi);
		
		Event ev = new Event();
		ev.widget = this;
		ev.data = mi;
		
		SelectionListener[] ls = listeners.toArray(new SelectionListener[listeners.size()]);
		for (SelectionListener l : ls) {
			l.widgetSelected(e);
		}
	}

	@Override
	public void widgetDisposed(DisposeEvent e) {
		unrealizeMenu();
	}

	private void unrealizeMenu() {
		for (MenuItem mi : menuItems) {
			mi.removeSelectionListener(this);
		}
		menuItems.clear();
	}

	public void addSelectionListener(SelectionListener listener) {
		if (listener == null)
			throw new NullPointerException();
		listeners.add(listener);
	}

	public MenuItem getSelectedItem() {
		return selectedItem;
	}

	public void setSelectedItem(MenuItem selectedItem) {
		this.selectedItem = selectedItem;
		if (selectedItem != null)
			button.setText(selectedItem.getText());
		else 
			button.setText("");
	}
}
