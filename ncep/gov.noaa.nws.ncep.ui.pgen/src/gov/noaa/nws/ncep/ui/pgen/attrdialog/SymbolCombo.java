/*
 * SymbolCombo
 * 
 * Date created: 29 JULY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import java.util.HashMap;

import gov.noaa.nws.ncep.ui.pgen.display.SymbolImageUtil;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

/**
 * @author bhebbard/sgilbert
 *
 * This widget allows users to select a GEMPAK symbol from a list of
 * possible symbols.
 * It uses a toolbar with a single drop-down button item that pops up
 * a menu, to simulate a Combo widget that works for images. 
 * 
 * Users can set a list of Symbol Ids to choose from, and if the Id
 * is recognized by the SymbolPatternManager, an icon of the image is created
 * and displayed in the drop down menu.  If the Id is unknown, the text string
 * is placed in the drop down menu in place of an icon.
 * 
 */
public class SymbolCombo extends Composite {
	
	private ToolBar tb;
	private ToolItem ti;
	private Menu menu;
	private Device device;
	private HashMap<String,Image> iconMap;
	
	private static final String NO_TEXT = new String("");
	
	/**
	 * Create a new SymbolCombo widget
	 * @param parent
	 */
    public SymbolCombo( Composite parent ) {
    	
    	super(parent.getParent(), SWT.NONE);
    	device = parent.getDisplay();
    	iconMap = new HashMap<String,Image>();
    	
    	tb = new ToolBar(parent, SWT.HORIZONTAL);
    	ti = new ToolItem(tb, SWT.DROP_DOWN);
    	menu = new Menu(parent.getShell(), SWT.POP_UP);
    	ti.addListener(SWT.Selection, new Listener() {
    		/* Main button clicked:  Pop up the menu showing all the symbols. */
    		public void handleEvent(Event event)
    		{
    			Rectangle bounds = ti.getBounds();
    			Point point = tb.toDisplay(bounds.x, bounds.y + bounds.height);
    			menu.setLocation(point);
    			menu.setVisible(true);
    		}
    	});
    	
    	/*
    	 * Dispose listener added to clean up icon images
    	 */
    	addDisposeListener(new DisposeListener() {
			@Override
			public void widgetDisposed(DisposeEvent e) {
				if ( iconMap != null ) {
					for ( Image im : iconMap.values() ) {
						im.dispose();
					}
					iconMap.clear();
				}
			}
    	});
    	
	}
    
    /**
     * Adds a ControlListener for changes in the toolbar
     */
    public void addControlListener(ControlListener listener) {
    	tb.addControlListener(listener);
    }
    
    /**
     * Set the list of symbols in the drop down menu
     * @param ids An array of symbol IDs
     */
    public void setItems( String[] ids ) {

    	removeAll();
    	
    	for (String id : ids ) {

    		// create new menu item
    		MenuItem mi = new MenuItem(menu, SWT.PUSH);
    		mi.setData(id);

    		if ( id != null ) {
    			// try generating an icon for the symbol ID
    			Image icon = SymbolImageUtil.createIcon(device,id);
    			
    			// Set the icon or text ID on the menu item
    			if ( icon != null ) {
        			icon.setBackground(tb.getBackground());
    				iconMap.put(id, icon);
        			mi.setImage(icon);
    			} else {
    	    		mi.setText(id);
    			}
    		}

    		mi.addListener(SWT.Selection, new Listener() {
    			/* A new value has been chosen off the pop-up menu:
     	           Set it back on the main toolItem button. */
    			public void handleEvent(Event event)
    			{
    				String text = (String) event.widget.getData();
    				setSelectedText(text);
    			}
    		});
    		
    	}
    	select(0);
    	tb.pack();
    }
    /**
     * Set the list of symbols in the drop down menu
     * @param ids An array of symbol IDs
	 * @param icons An array of icon images
     */
    public void setItems( String[] ids, Image[] icons) {

    	removeAll();
    	
    	for (int ii= 0; ii < ids.length; ii ++ ) {

    		// create new menu item
    		MenuItem mi = new MenuItem(menu, SWT.PUSH);
    		mi.setData(ids[ii]);
    		
    			// Set the icon or text ID on the menu item
    			if ( icons[ii] != null ) {
        		//	icon.setBackground(tb.getBackground());
    				iconMap.put(ids[ii], icons[ii]);
        			mi.setImage(icons[ii]);
        			
    			} else {
    	    		mi.setText(ids[ii]);
    			}
    			
    		mi.addListener(SWT.Selection, new Listener() {
    			/* A new value has been chosen off the pop-up menu:
     	           Set it back on the main toolItem button. */
    			public void handleEvent(Event event)
    			{
    				String text = (String) event.widget.getData();
    				setSelectedText(text);
    			}
    		});
    		
    	}
    	select(0);
    	      
    	tb.pack();
    }
    /**
     * Remove the current items from the drop down menu
     */
	public void removeAll() {
		
		for ( MenuItem mi: menu.getItems() ) {
			mi.dispose();
		}
		tb.pack();
		
	}
	
	/**
	 * Select an item from the possible list by index
	 * @param index
	 */
	public void select(int index) {
	
		MenuItem[] items = menu.getItems(); 
		if ( index >= 0 && index < items.length ) {
			String text = (String)items[index].getData();
			setSelectedText(text);
		}
		
	}
	
	/**
	 * get the symbol ID of the currently selected item
	 * @return
	 */
	public String getSelectedText() {
		return (String)ti.getData();
	}
	
	/**
	 * Select the item with the given symbol ID.
	 * @param text
	 */
	public void setSelectedText(String text) {
		
		ti.setData(text);
		/*
		 * if an icon exists for this symbol ID, use it.
		 * Otherwise set the ID text in the menu
		 */
		if ( iconMap.containsKey(text) ) {
			ti.setText(NO_TEXT);
			ti.setImage(iconMap.get(text));
		}
		else {
			ti.setText(text);
			ti.setImage(null);
		}
		tb.pack();
	}
	
	/**
	 * Disable or enable widget
	 */
	public void setEnabled(boolean enable) {
		
		if ( ! enable ) {
			String disable = new String("Not Applicable");
			ti.setData(disable);
			ti.setText(disable);
			ti.setImage(null);
		}
		
		tb.setEnabled(enable);
		tb.pack();
		
	}
	
}
