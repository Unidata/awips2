/*
 * Created on Mar 7, 2007
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package ohd.hseb.raxbase.util;

import java.awt.Color;
import java.awt.Component;
import java.util.List;

import javax.swing.DefaultListCellRenderer;
import javax.swing.JLabel;
import javax.swing.JList;

import ohd.hseb.rfc.util.batch.BatchProcessor;
import ohd.hseb.rfc.util.batch.SystemSettings;
import ohd.hseb.rfc.util.listpanels.JListPanel;

/**
 * Wrapper on DefaultListCellRenderer; it turns the backgroun to a color if the specified cell
 * corresponds to a location that has data.
 * @author hank
 */
public class JListPanelSuperCellRenderer extends DefaultListCellRenderer 
{
    private static final long serialVersionUID = 1L;
    final static String CLASSNAME="JListPanelCellRenderer";
    
    public final static Color DEFAULT_LIST_ITEM_BG_COLOR = Color.WHITE;
    public final static Color DEFAULT_LIST_ITEM_SELECTED_BG_COLOR = Color.YELLOW;
    
    /**
     * The list of items to highlight, by name.
     */
    private List<String> _listItemsHighlighted;
    
    /**
     * The color for highlighted items.
     */
    private Color _highlightBGColor;

    /**
     * The color for non-highlighted items.
     */
    private Color _listItemBGColor;
    
    /**
     * The panel that this renderer is attached to.
     */
    private JListPanel _panelBeingRendered;
    
    /**
     * Text added to components (JLabel) that are "highlighted".
     * Default value is "".
     */
    private String _highlightedPrefixText;
    
    /**
     * Text added to components (JLabel) that are not "highlighted".
     * Default value is "".
     */
    private String _notHighlightedPrefixText;

    
    /**
     * @param panel  The panel to which this renderer is attached.
     * @param listItemsHighlighted List of String (MUST BE Strings!)
     * @param listItemBGColor BG color for non-highlighted
     * @param highlightBGColor BG color for highlighted.
     */
    public JListPanelSuperCellRenderer(
        JListPanel panel, 
        List listItemsHighlighted)
    {
        _panelBeingRendered = panel;
        _listItemsHighlighted = listItemsHighlighted;
        _highlightedPrefixText = "";
        _notHighlightedPrefixText = "";
        this.loadColors("", "");
    }
    
    
    /**
     * @param panel  The panel to which this renderer is attached.
     * @param listItemsHighlighted List of String (MUST BE Strings!)
     * @param listItemBGColor BG color for non-highlighted
     * @param highlightBGColor BG color for highlighted.
     */
    public JListPanelSuperCellRenderer(
        JListPanel panel, 
        List listItemsHighlighted, 
        String listItemBGSystemSetting, 
        String highlightBGSystemSetting)
    {
        _panelBeingRendered = panel;
        _listItemsHighlighted = listItemsHighlighted;
        _highlightedPrefixText = "";
        _notHighlightedPrefixText = "";
        this.loadColors(listItemBGSystemSetting, highlightBGSystemSetting);
    }
    
    
    private void loadColors(String listItemBGSystemSetting, String listItemSelectedBGSystemSetting)
    {
        _listItemBGColor = SystemSettings.retrieveSystemSettingColor(
            listItemBGSystemSetting, DEFAULT_LIST_ITEM_BG_COLOR);
        _highlightBGColor = SystemSettings.retrieveSystemSettingColor(
            listItemSelectedBGSystemSetting, DEFAULT_LIST_ITEM_SELECTED_BG_COLOR);
    }
    
    
    /**
     * Override DefaultListCellRenderer.
     */
    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus)
    {
        Component comp = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

        comp.setForeground( Color.BLUE );
        //Deal with the prefix
        if (!((String)value).equalsIgnoreCase(BatchProcessor.NONE_STR))
        {
            if ((_listItemsHighlighted == null) ||
                (_listItemsHighlighted.indexOf((String)value) < 0))
            {
                ((JLabel)comp).setText(_notHighlightedPrefixText + ((JLabel)comp).getText());
            }
            else
            {
                ((JLabel)comp).setText(_highlightedPrefixText + ((JLabel)comp).getText());
            }
        }
        
        //Deal with the background color.
        if (_listItemsHighlighted == null)
        {
            return comp;
        }
        if ((_listItemsHighlighted.indexOf((String)value) >= 0) && (!isSelected))
        {
            comp.setBackground(_highlightBGColor);
        }
        else if (!isSelected)
        {
            comp.setBackground(_listItemBGColor);
        }
        return comp;
    }
    
    public void setHighlightedPrefixText(String str)
    {
        this._highlightedPrefixText = str;
    }
    public void setNotHightlightedPrefixText(String str)
    {
        this._notHighlightedPrefixText = str;
    }
    public JListPanel getPanelBeingRendered()
    {
        return this._panelBeingRendered;
    }

}
