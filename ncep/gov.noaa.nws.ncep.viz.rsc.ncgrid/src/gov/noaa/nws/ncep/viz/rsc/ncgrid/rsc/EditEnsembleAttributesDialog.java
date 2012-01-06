package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.AbstractEditResourceAttrsDialog;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

/**
 * The grid contour attribute editing dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Oct  2010    277			 M. Li		Copied form EditGridAttributeDialog
 * Nov,22 2010  352			 X. Guo     Add HILO, HLSYM and move all help functions
 *                                      into NcgridAttributesHelp.java
 * 
 * @author mli
 * @version 1
 */

public class EditEnsembleAttributesDialog extends AbstractEditResourceAttrsDialog {
    private RscAttrValue cintString = null;
    private RscAttrValue gdfile = null;
    private RscAttrValue glevel = null;
    private RscAttrValue gvcord = null;
    private RscAttrValue scale = null;
    private RscAttrValue gdpfun = null;
    private RscAttrValue type = null;
    private RscAttrValue lineAttr = null;
    private RscAttrValue fint = null;
    private RscAttrValue fline = null;
    private RscAttrValue hilo = null;
    private RscAttrValue hlsym = null;
    private RscAttrValue wind = null;
    private RscAttrValue title = null;
    private RscAttrValue colors = null;
    private RscAttrValue marker = null;
    private RscAttrValue grdlbl = null;
    
    private Text gdfileText;
    private Text glevelText;
    private Text gvcordText;
    
    private Text scaleText;
    private Text gdpfunText;
    private Text typeText;
    private Text cintText;
    private Text lineAttrText;
    private Text fintAttrText;
    private Text flineAttrText;
    private Text hiloAttrText;
    private Text hlsymAttrText;
    private Text windAttrText;
    private Text titleAttrText;
    private Text colorsText;
    private Text markerText;
    private Text grdlblText;
	
    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public EditEnsembleAttributesDialog(Shell parentShell, INatlCntrsResourceData rd, Boolean apply ) {
        super(parentShell, rd, apply);
    }

    @Override
    public Composite createDialog(Composite composite) {
    	
    	lineAttr   = editedRscAttrSet.getRscAttr("lineAttributes");
    	cintString = editedRscAttrSet.getRscAttr("cint");
    	gdfile     = editedRscAttrSet.getRscAttr("gdfile");
    	glevel     = editedRscAttrSet.getRscAttr("glevel");
    	gvcord     = editedRscAttrSet.getRscAttr("gvcord");
    	scale      = editedRscAttrSet.getRscAttr("scale");
    	gdpfun     = editedRscAttrSet.getRscAttr("gdpfun");
    	type       = editedRscAttrSet.getRscAttr("type");
    	fint       = editedRscAttrSet.getRscAttr("fint");
    	fline      = editedRscAttrSet.getRscAttr("fline");
    	hilo       = editedRscAttrSet.getRscAttr("hilo");
    	hlsym      = editedRscAttrSet.getRscAttr("hlsym");
    	title      = editedRscAttrSet.getRscAttr("title");
    	colors     = editedRscAttrSet.getRscAttr("colors");
    	marker     = editedRscAttrSet.getRscAttr("marker");
    	grdlbl     = editedRscAttrSet.getRscAttr("grdlbl");
    	wind      = editedRscAttrSet.getRscAttr("wind");
    	
    	
    	// confirm the classes of the attributes..
    	if( lineAttr.getAttrClass() != String.class ) {
    		System.out.println( "line is not of expected class? "+ lineAttr.getAttrClass().toString() );
    	}
    	if (cintString.getAttrClass() != String.class ){
    		System.out.println( "cint is not of expected class? "+ cintString.getAttrClass().toString() );
    	}
    	
    	if (cintString == null 
    			|| ((String)cintString.getAttrValue()).trim().length() <= 0)
    		cintString.setAttrValue((String)"");
    	
    	if (hilo != null 
    			&& ((String)hilo.getAttrValue()).trim().length() <= 0)
    		hilo.setAttrValue((String)"");

    	if (hlsym != null 
    			&& ((String)hlsym.getAttrValue()).trim().length() <= 0)
    		hlsym.setAttrValue((String)"");
    	
		//contour attributes editing
        Group contourAttributesGroup = new Group ( composite, SWT.SHADOW_NONE );
        GridLayout contourAttrGridLayout = new GridLayout();
        contourAttrGridLayout.numColumns = 1;
        contourAttrGridLayout.marginHeight = 8;
        contourAttrGridLayout.marginWidth = 2;
        contourAttrGridLayout.horizontalSpacing = 20;
        contourAttrGridLayout.verticalSpacing = 8;
        contourAttributesGroup.setLayout(contourAttrGridLayout);
        
       
        Composite comp = new Composite(contourAttributesGroup, SWT.SHADOW_NONE); 
        GridLayout contourIntervalsGridLayout = new GridLayout();
        contourIntervalsGridLayout.numColumns = 2;
        comp.setLayout(contourIntervalsGridLayout); 
        
     // GDFILE
        Button gdfileButton = new Button(comp, SWT.PUSH);
        gdfileButton.setText("GDFILE:");
        gdfileButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
       		    EnsembleSelectDialog ensembleSelectDialog = new EnsembleSelectDialog(shell, gdfileText.getText());
       		    if (!(ensembleSelectDialog.isOpen())) ensembleSelectDialog.open();
       		    if (ensembleSelectDialog.getSelectedModels() != null && 
            				 !ensembleSelectDialog.getSelectedModels().equalsIgnoreCase(gdfileText.getText())) {
       		    	gdfileText.setText(ensembleSelectDialog.getSelectedModels());
       		    }
            }
        });
        
        gdfileText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        gdfileText.setLayoutData(new GridData(300, SWT.DEFAULT));
        gdfileText.setText((String)gdfile.getAttrValue());
        gdfileText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				gdfile.setAttrValue((String)gdfileText.getText().trim());
			}
        	
        });
        
        // GLEVEL
        Label glevelLabel = new Label(comp, SWT.NONE);
        glevelLabel.setText("GLEVEL:");
        glevelText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        glevelText.setLayoutData(new GridData(300, SWT.DEFAULT));
        glevelText.setText((String)glevel.getAttrValue());
        glevelText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				glevel.setAttrValue((String)glevelText.getText().trim());
			}
        	
        });
        
        // GVCORD
        Label gvcordLabel = new Label(comp, SWT.NONE);
        gvcordLabel.setText("GVCORD:");
        gvcordText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        gvcordText.setLayoutData(new GridData(300, SWT.DEFAULT));
        gvcordText.setText((String)gvcord.getAttrValue());
        gvcordText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				gvcord.setAttrValue((String)gvcordText.getText().trim());
			}
        });
        
        // SCALE
        Label scaleLabel = new Label(comp, SWT.NONE);
        scaleLabel.setText("SCALE:");
        scaleText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        scaleText.setLayoutData(new GridData(300, SWT.DEFAULT));
        scaleText.setText((String)scale.getAttrValue());
        scaleText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				scale.setAttrValue((String)scaleText.getText().trim());
			}
        });
        
        
        // GDPFUN
        Label gdpfunLabel = new Label(comp, SWT.NONE);
        gdpfunLabel.setText("GDPFUN:");
        gdpfunText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        gdpfunText.setLayoutData(new GridData(300, SWT.DEFAULT));
        gdpfunText.setText((String)gdpfun.getAttrValue());
        gdpfunText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				gdpfun.setAttrValue((String)gdpfunText.getText().trim());
			}
        });
        
        // TYPE
        Label typeLabel = new Label(comp, SWT.NONE);
        typeLabel.setText("TYPE:");
        typeText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        typeText.setLayoutData(new GridData(300, SWT.DEFAULT));
        typeText.setText((String)type.getAttrValue());
        typeText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				type.setAttrValue((String)typeText.getText().trim());
			}
        });
        
                            
        // Contour Intervals -- CINT
        Label contourIntervalsLabel = new Label(comp, SWT.NONE);
        contourIntervalsLabel.setText("CINT:");
        cintText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        cintText.setLayoutData(new GridData(300, SWT.DEFAULT));
        cintText.setText((String)cintString.getAttrValue());
        cintText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				cintString.setAttrValue((String)cintText.getText().trim());
			}
        	
        });
        
     // LINE
        Label lineAttrLabel = new Label(comp, SWT.NONE);
        lineAttrLabel.setText("LINE:");
        lineAttrText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        lineAttrText.setLayoutData(new GridData(300, SWT.DEFAULT));
        lineAttrText.setText((String)lineAttr.getAttrValue());
        lineAttrText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				lineAttr.setAttrValue((String)lineAttrText.getText().trim());
			}
        });
        
        // FINT
        Label fintAttrLabel = new Label(comp, SWT.NONE);
        fintAttrLabel.setText("FINT:");
        fintAttrText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        fintAttrText.setLayoutData(new GridData(300, SWT.DEFAULT));
        fintAttrText.setText((String)fint.getAttrValue());
        fintAttrText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				fint.setAttrValue((String)fintAttrText.getText().trim());
			}
        });
        
        // FLINE
        Label flineAttrLabel = new Label(comp, SWT.NONE);
        flineAttrLabel.setText("FLINE:");
        flineAttrText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        flineAttrText.setLayoutData(new GridData(300, SWT.DEFAULT));
        flineAttrText.setText((String)fline.getAttrValue());
        flineAttrText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				fline.setAttrValue((String)flineAttrText.getText().trim());
			}
        });
        
     // HILO
        if ( hilo != null ) {
        	Label hiloAttrLabel = new Label(comp, SWT.NONE);
        	hiloAttrLabel.setText("HILO:");
        	hiloAttrText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        	hiloAttrText.setLayoutData(new GridData(300, SWT.DEFAULT));
        	hiloAttrText.setText((String)hilo.getAttrValue());
        	hiloAttrText.addModifyListener(new ModifyListener() {
        		public void modifyText(ModifyEvent e) {
        			hilo.setAttrValue((String)hiloAttrText.getText().trim());
        		}
        	});
        }
        
     // HLSYM
        if ( hlsym != null ) {
        	Label hlsymAttrLabel = new Label(comp, SWT.NONE);
        	hlsymAttrLabel.setText("HLSYM:");
        	hlsymAttrText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        	hlsymAttrText.setLayoutData(new GridData(300, SWT.DEFAULT));
        	hlsymAttrText.setText((String)hlsym.getAttrValue());
        	hlsymAttrText.addModifyListener(new ModifyListener() {
        		public void modifyText(ModifyEvent e) {
        			hlsym.setAttrValue((String)hlsymAttrText.getText().trim());
        		}
        	});
        }
        
     // WIND
        Label windAttrLabel = new Label(comp, SWT.NONE);
        windAttrLabel.setText("WIND:");
        windAttrText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        windAttrText.setLayoutData(new GridData(230, SWT.DEFAULT));
        windAttrText.setText((String)wind.getAttrValue());
        windAttrText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				wind.setAttrValue((String)windAttrText.getText().trim());
			}
        });
        
        // TITLE
        Label titleAttrLabel = new Label(comp, SWT.NONE);
        titleAttrLabel.setText("TITLE:");
        titleAttrText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        titleAttrText.setLayoutData(new GridData(300, SWT.DEFAULT));
        titleAttrText.setText((String)title.getAttrValue());
        titleAttrText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				title.setAttrValue((String)titleAttrText.getText().trim());
			}
        });
        
        
        // COLORS
        Label colorsLabel = new Label(comp, SWT.NONE);
        colorsLabel.setText("COLORS:");
        colorsText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        colorsText.setLayoutData(new GridData(300, SWT.DEFAULT));
        colorsText.setText((String)colors.getAttrValue());
        colorsText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				colors.setAttrValue((String)colorsText.getText().trim());
			}
        });
        
        // MARKER
        Label markerLabel = new Label(comp, SWT.NONE);
        markerLabel.setText("MARKER:");
        markerText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        markerText.setLayoutData(new GridData(300, SWT.DEFAULT));
        markerText.setText((String)marker.getAttrValue());
        markerText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				marker.setAttrValue((String)markerText.getText().trim());
			}
        });
        
        // GRDLBL
        Label grdlblLabel = new Label(comp, SWT.NONE);
        grdlblLabel.setText("GRDLBL:");
        grdlblText = new Text(comp,SWT.SINGLE | SWT.BORDER );
        grdlblText.setLayoutData(new GridData(300, SWT.DEFAULT));
        grdlblText.setText(String.valueOf((Integer)grdlbl.getAttrValue()));
        grdlblText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				grdlbl.setAttrValue((Integer)Integer.valueOf(grdlblText.getText().trim()));
			}
        });
        
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(contourAttributesGroup, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
        
        final Button toolTipDisplay = new Button(contourAttributesGroup, SWT.CHECK);
        toolTipDisplay.setLayoutData(new GridData(SWT.DEFAULT, SWT.DEFAULT));
        toolTipDisplay.setText("ToolTips OFF");
        toolTipDisplay.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	if (toolTipDisplay.getSelection()) {
            		toolTipDisplay.setText("ToolTips ON");
            		EnableToolTip(true);
            	}
            	else {
            		toolTipDisplay.setText("ToolTips OFF");
            		EnableToolTip(false);
            	}
            }
        });
        return composite;
    }
    
    private void EnableToolTip(boolean on) {
    	if (on) {
    		glevelText.setToolTipText(NcgridAttributesHelp.GlevelToolTipText());
    		gvcordText.setToolTipText(NcgridAttributesHelp.GvcordToolTipText());
    		scaleText.setToolTipText(NcgridAttributesHelp.ScaleToolTipText());
    		gdpfunText.setToolTipText(NcgridAttributesHelp.GdpfunToolTipText());
    		typeText.setToolTipText(NcgridAttributesHelp.TypeToolTipText());
    		cintText.setToolTipText(NcgridAttributesHelp.CintToolTipText());
    		lineAttrText.setToolTipText(NcgridAttributesHelp.LineToolTipText());
    		fintAttrText.setToolTipText(NcgridAttributesHelp.FintToolTipText());
    		flineAttrText.setToolTipText(NcgridAttributesHelp.FlineToolTipText());
    		if ( hilo != null ) {
    			hiloAttrText.setToolTipText(NcgridAttributesHelp.HiloToolTipText());
    		}
    		if ( hlsym != null ) {
    			hlsymAttrText.setToolTipText(NcgridAttributesHelp.HlsymToolTipText());
    		}
    		titleAttrText.setToolTipText(NcgridAttributesHelp.TitleToolTipText());
    		colorsText.setToolTipText(NcgridAttributesHelp.ColorsToolTipText());
    		markerText.setToolTipText(NcgridAttributesHelp.MarkerToolTipText());
    		grdlblText.setToolTipText(NcgridAttributesHelp.GrdlblToolTipText());
    		windAttrText.setToolTipText(NcgridAttributesHelp.WindToolTipText());
    	}
    	else {
    		glevelText.setToolTipText(null);
    		gvcordText.setToolTipText(null);
    		scaleText.setToolTipText(null);
    		gdpfunText.setToolTipText(null);
    		typeText.setToolTipText(null);
    		cintText.setToolTipText(null);
    		lineAttrText.setToolTipText(null);
    		fintAttrText.setToolTipText(null);
    		flineAttrText.setToolTipText(null);
    		if ( hilo != null ) {
    			hiloAttrText.setToolTipText(null);
    		}
    		if ( hlsym != null ) {
    			hlsymAttrText.setToolTipText(null);
    		}
    		titleAttrText.setToolTipText(null);
    		colorsText.setToolTipText(null);
    		markerText.setToolTipText(null);
    		grdlblText.setToolTipText(null);
    		windAttrText.setToolTipText(null);
    	}
    }

	@Override
	public void initWidgets() {
		// TODO Auto-generated method stub
	}
	
	
}

