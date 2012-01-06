package ohd.hseb.monitor;

import java.awt.Component;

import javax.swing.JSplitPane;

public class MonitorSplitPane extends JSplitPane
{

    /**
    * 
    */
   private static final long serialVersionUID = 125315154454l;

   public MonitorSplitPane (int newOrientation,
            boolean newContinuousLayout)
    {
        super(newOrientation, newContinuousLayout);
    }

    public void setDividerLocation(int location)
    {
        int currentDividerLocation = getDividerLocation();
        int adjustedLocation = location;


        if (location < 30 )
        {
            int difference = currentDividerLocation - location;
            if (difference > 50)
            {
                adjustedLocation = currentDividerLocation;
            }
        }
        super.setDividerLocation(adjustedLocation);
    }

    public void setRightComponent(Component comp)
    {
    
        int beforeDividerLocation = getDividerLocation();
     
        super.setRightComponent(comp);

        setDividerLocation(beforeDividerLocation);
        
    }

    public void setLeftComponent(Component comp)
    {
        String header = "MonitorJSplitPane.setLeftComponent() ";

        int beforeDividerLocation = getDividerLocation();
    
        super.setLeftComponent(comp);

        setDividerLocation(beforeDividerLocation);

    }

}

