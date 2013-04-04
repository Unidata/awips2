package gov.noaa.nws.ncep.viz.rsc.plotdata.advanced;

import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModelElement;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/**
 *  UI for editing ConditionalColorBar for Conditional Coloring  of Plot Model Elements
 *   
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/2012      #431       S. Gurung   Initial Creation.
 *  
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class EditConditionalColorbarComposite extends Composite {	
	
    private PlotModelElement editedPlotModelElement = null;
  
    private Composite topComposite = null;
    
    public EditConditionalColorbarComposite( Composite parent, int style, PlotModelElement cf) {
		super(parent, style);
		editedPlotModelElement = cf;
	
		topComposite = this;
		
		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 1;
		mainLayout.marginWidth = 1;
		mainLayout.verticalSpacing = 5;
		topComposite.setLayout(mainLayout);

		createControls();
		
		initWidgets();
	}
	
	private void createControls() {
		if( editedPlotModelElement == null ) {
			System.out.println("PlotModelElement to Edit is not set???");
			return;
		}				
		
		Group colorBarGrp = new Group( topComposite, SWT.NONE );
        colorBarGrp.setText(" Conditional Color Bar");
        colorBarGrp.setLayout( new FormLayout() );
	        
        new ConditionalColorBarEditor( colorBarGrp, editedPlotModelElement.getConditionalColorBar() );     
				
	}

	public void initWidgets() {	
		
	}
	
	public PlotModelElement getEditedPlotModelElement() {
		return editedPlotModelElement;
	}	
	 
	public ConditionalColorBar getEditedConditionalColorBar() {
		return editedPlotModelElement.getConditionalColorBar();
	}	   
	
}

