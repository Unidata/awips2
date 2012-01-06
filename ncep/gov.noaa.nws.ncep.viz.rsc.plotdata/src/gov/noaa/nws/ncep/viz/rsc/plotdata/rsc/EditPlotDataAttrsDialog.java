package gov.noaa.nws.ncep.viz.rsc.plotdata.rsc;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.rsc.plotdata.plotModels.elements.PlotModel;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;

/**
 *  UI for editing Point data resource attributes. 
 *   
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/15/2009    172       	M. Li  		Initial creation.
 * 12/05/2009    xxx        Greg Hull   
 * 04/27/2010    #245       Greg Hull    Added Apply Button
 * 07/26/2010    T285       qzhou       Modified createDialog for uair
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 */
public class EditPlotDataAttrsDialog extends AbstractEditResourceAttrsDialog {
	private RscAttrValue plotLevelAttr = null;
	private RscAttrValue plotModelAttr = null;
	private PlotModel editedPlotModel = null;
	
	private boolean isSounding = false;
	private Combo levelCombo = null;
	private final String[] uairOptions = {"1000", "925", "850", "700", "500", "400", "300", "250", "200", "150", "100"}; //, "70", "50", "30", "20", "10", "7", "5", "3", "2", "1"};

	public EditPlotDataAttrsDialog(Shell parentShell, INatlCntrsResourceData r, Boolean apply) {
		super(parentShell, r, apply);
		if( !(r instanceof PlotResourceData ) ) {
			System.out.println("EditPlotDataAttrsDialog: Resource is not a PlotResource");
			return;
		}
		
		isSounding = !((PlotResourceData)r).isSurfaceOnly();				
	}

	@Override
	public Composite createDialog(Composite topComp) {

		// get the category from the qualified resource name (the plotModel category 
		// is the type in the resource organizational structure. ie. METAR, SHIP, ...)
		// 
		//			String rscCat = NmapCommon.getResourceType( rscData.getQualifiedResourceName() );
		//			editedPlotModel = plotModelMngr.getPlotModel( rscCat, 
		//					((PlotResourceData)rscData).plotModelName );
		if (isSounding) {
			plotLevelAttr = editedRscAttrSet.getRscAttr( "levelKey" );
			if( plotLevelAttr == null || plotLevelAttr.getAttrClass() != String.class ) {
				System.out.println("plotModelAttr is null or not of expected class plotModel?");
			return topComp;
			}
		}
		
		plotModelAttr = editedRscAttrSet.getRscAttr( "plotModel" );

//		editedPlotModel =  new PlotModel( (PlotModel)plotModelAttr.getAttrValue() );
		// point to the actual object in the editedRscAttrSet so that changes made by the
		// EditPlotModelComposite will be reflected in the editedPlotModel and will
		// be updated when ok is clicked.
		editedPlotModel = (PlotModel)plotModelAttr.getAttrValue();

		if( plotModelAttr == null || plotModelAttr.getAttrClass() != PlotModel.class ) {
			System.out.println("plotModelAttr is null or not of expected class plotModel?");
			return topComp;
		}
		
		
		if (isSounding) {
			Group hightAttrGrp = new Group ( topComp, SWT.SHADOW_NONE );
			hightAttrGrp.setLayout(new GridLayout(2, false));
			hightAttrGrp.setText("Vertical Level(mb)");
			
			levelCombo = new Combo(hightAttrGrp, SWT.DROP_DOWN | SWT.READ_ONLY );
			//levelCombo = new Combo(hightAttrGrp, SWT.DROP_DOWN );
			levelCombo.setItems(uairOptions);
			RscAttrValue ra = editedRscAttrSet.getRscAttr("levelKey");
			String s = ra.getAttrValue().toString();
			
			if (s.equalsIgnoreCase("1000"))
				levelCombo.select(0);
			else if (s.equalsIgnoreCase("925"))
				levelCombo.select(1);
			else if (s.equalsIgnoreCase("850"))
				levelCombo.select(2);
			else if (s.equalsIgnoreCase("700"))
				levelCombo.select(3);
			else if (s.equalsIgnoreCase("500"))
				levelCombo.select(4);
			else if (s.equalsIgnoreCase("400"))
				levelCombo.select(5);
			else if (s.equalsIgnoreCase("300"))
				levelCombo.select(6);
			else if (s.equalsIgnoreCase("250"))
				levelCombo.select(7);
			else if (s.equalsIgnoreCase("200"))
				levelCombo.select(8);
			else if (s.equalsIgnoreCase("150"))
				levelCombo.select(9);
			else if (s.equalsIgnoreCase("100"))
				levelCombo.select(10);
			
			levelCombo.addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent event) {
					// TODO Auto-generated method stub
					String s = levelCombo.getText();
					editedRscAttrSet.setAttrValue("levelKey", s);
				}
			});
	//		levelCombo.addModifyListener(new ModifyListener() {			
	//			@Override
	//			public void modifyText(ModifyEvent e) {
	//				// TODO Auto-generated method stub
	//				String s = levelCombo.getText();
	//				editedRscAttrSet.setAttrValue("levelKey", s);
	//			}
	//		});
		}
		
		Composite editPlotModelComp = new EditPlotModelComposite(
				     topComp, SWT.NONE, editedPlotModel, this.rscData );

		return topComp;
	}		
				
	@Override
	public void initWidgets() {
	}
}
