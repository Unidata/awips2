package ohd.hseb.monitor.messaging;

public enum MessageType
{
    RELOAD_DATA,    // Read data from database. Sent by: lookback time menu filters and refresh/update now button. Received by refresh mgr.
    
    REFRESH_DISPLAY, // Read filter and menu settings and refresh display. Sent by filter mgr, when the check box is 
                     // clicked and by menu mgr when the missing filter is changed
    
    RELOAD_DATA_WITH_NEW_SETTINGS, // Sent by settings mgr, when new settings are loaded. Received by Refresh mgr
    
    UPDATE_DISPLAY_WITH_SETTINGS, // Sent by Refresh mgr, when new settings are loaded. Received by view,menu & filter mgr and they are updated
                                  // based on the settings
    
    LOAD_SETTINGS, // sent by load settings menu and received by settings mgr
    
    LOAD_OFFICE_SETTINGS, // sent by load office settings menu and received by settings mgr
    
    SAVE_SETTINGS,// sent by save settings menu and received by settings mgr
    
    SAVE_OFFICE_SETTINGS,// sent by save office settings menu and received by settings mgr
    
    FILTER_SELECT_ITEM, //sent when an location in the filter tree is highlighted; Received by view mgr to highlight the row in table
    
    VIEW_SELECT_ITEM, // sent by view mgr when a row is selected in the table along with the row data;
                        // Received by menu mgr to get the corresponding row data
    
    CREATE_TEXT_FROM_VIEW, // sent by export table to text menu, received by view manager
    
    UPDATE_SETTINGS, // sent by settings mgr before saving the settings to file, 
                    // received by view mgr to set the current table settings in view settings
    
    CHANGE_COLUMNS, // sent by column change menu, received by view mgr
    
    APP_SORT, // sent by hsa-group-location sort menu, received by view mgr
    
    RIVER_THREAT_SORT, // sent by threat sort menu, received by view mgr
    
    PRECIP_THREAT_SORT,// sent by precip threat sort menu, received by view mgr
    
    CLEAR_SORT; // sent by clear sort menu, received by view mgr
}
