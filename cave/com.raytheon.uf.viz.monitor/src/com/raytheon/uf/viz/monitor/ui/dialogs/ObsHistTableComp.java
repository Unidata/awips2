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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.ObsHistType;
import com.raytheon.uf.viz.monitor.data.ColumnAttribData;
import com.raytheon.uf.viz.monitor.data.TableData;

/**
 * Observation History table composite.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ObsHistTableComp extends TableComp
{
    /**
     * Common table configuration data.
     */
    private CommonTableConfig tableConfig;
    
    /**
     * Observation type.
     */
    private ObsHistType obsType;
    
    /**
     * Constructor.
     * @param parent Parent composite.
     * @param data Table data.
     * @param appName Application name.
     * @param obsType Observation type.
     */
    public ObsHistTableComp(Composite parent, TableData data, CommonConfig.AppName appName,
            ObsHistType obsType)
    {
        super(parent, data, appName);
        
        this.obsType = obsType;
        
        tableConfig = CommonTableConfig.getInstance();  
        
        init();
    }
    
    /**
     * Method not used.
     */
    @Override
    protected void addTopTableControls(Composite parentComp)
    {
        // Don't need to do anything here...
    }

    /**
     * Method not used.
     */
    @Override
    protected void tableMouseDownAction(MouseEvent event)
    {
        // Don't need to do anything here...
    }

    /**
     * Method not used.
     */
    @Override
    protected void tableMouseHoverAction(MouseEvent event)
    {
       // Don't need to do anything here...
    }

    /**
     * Get the observation history column index.
     * @param appName Application name.
     * @param sortCol Column to sort on.
     * @return The column index of the column to be sorted.
     */
    @Override
    protected int getColumnIndex(AppName appName, String sortCol)
    {
        return tableConfig.getObsHistColumnIndex(appName, obsType, sortCol);
    }

    /**
     * Get the array of column keys used to access the column metadata.
     * @param app Application name.
     * @return String array of column keys.
     */
    @Override
    protected String[] getColumnKeys(AppName app)
    {
        return tableConfig.getObsHistColumnKeys(app, obsType);
    }

    /**
     * Get the default column width.
     * @param appName Application name.
     * @return Default column width.
     */
    @Override
    protected int getDefaultColWidth(AppName appName)
    {
        return tableConfig.getTableDefaultColWidth(appName);
    }

    /**
     * Get the table attribute data for the specified column. 
     * @param colName Column name.
     * @return The attribute data.
     */
    @Override
    protected ColumnAttribData getColumnAttribteData(String colName)
    {
        return tableConfig.getObsHistColumnAttr(colName);
    }

    @Override
    protected void tableColRightMouseAction(MouseEvent event)
    {
        // TODO Auto-generated method stub
        
    }

	@Override
	protected void packColumns() {
	      for (int i = 0; i < table.getColumnCount(); i++) {
	          table.getColumn(i).pack();
	          table.getColumn(i).setWidth(table.getColumn(i).getWidth() + 5);
	      }
	}
}
