package gov.noaa.gsd.viz.ensemble.util;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.ColorDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * This class is a Dialog which allows users to change the colors of the GFS
 * ensemble perturbation members. It is used as a convenience feature to make it
 * easy to create a gradient of colors given a base color.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2014    5056      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class EnsembleGEFSColorChooser extends CaveJFACEDialog {

    /**
     * Create the dialog.
     * 
     * @param parentShell
     */
    public EnsembleGEFSColorChooser(Shell parentShell) {
        super(parentShell);
    }

    /**
     * Create contents of the dialog.
     * 
     * @param parent
     */
    @Override
    protected Control createDialogArea(Composite parent) {

        Composite container = (Composite) super.createDialogArea(parent);
        GridLayout gridLayout = (GridLayout) container.getLayout();
        gridLayout.numColumns = 5;
        gridLayout.makeColumnsEqualWidth = false;

        Label label_modelName_GEFS = new Label(container, SWT.BORDER
                | SWT.CENTER);
        label_modelName_GEFS.setAlignment(SWT.CENTER);
        GridData gd_label_modelName_GEFS = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 1, 1);
        gd_label_modelName_GEFS.heightHint = 20;
        gd_label_modelName_GEFS.widthHint = 40;
        label_modelName_GEFS.setLayoutData(gd_label_modelName_GEFS);
        label_modelName_GEFS.setText("GEFS");

        Label label_colon_GEFS = new Label(container, SWT.NONE);
        label_colon_GEFS.setText(":");

        final Composite label_color_GEFS = new Composite(container, SWT.BORDER);
        label_color_GEFS.setForeground(ChosenGEFSColors.getInstance()
                .getColor());
        label_color_GEFS.setBackground(SWTResourceManager.WHITE);
        GridData gd_label_color_GEFS = new GridData(SWT.LEFT, SWT.CENTER,
                false, false, 3, 1);
        gd_label_color_GEFS.heightHint = 24;
        gd_label_color_GEFS.widthHint = 116;
        gd_label_color_GEFS.minimumWidth = 116;
        gd_label_color_GEFS.minimumHeight = 24;
        label_color_GEFS.setLayoutData(gd_label_color_GEFS);
        label_color_GEFS.setSize(116, 24);
        applyGradientBG(label_color_GEFS);
        label_color_GEFS.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {

                ColorDialog cd = new ColorDialog(getShell());
                cd.setRGB(ChosenGEFSColors.getInstance().getColor().getRGB());
                cd.setText("Choose GEFS lower color");
                RGB result = cd.open();
                if (result != null) {
                    Color c = SWTResourceManager.getColor(result);
                    RGB rgb = c.getRGB();
                    float[] hsb = rgb.getHSB();
                    hsb[1] = 1.0f;
                    RGB nrgb = new RGB(hsb[0], hsb[1], hsb[2]);
                    Color nc = SWTResourceManager.getColor(nrgb);
                    ChosenGEFSColors.getInstance().setColor(nc);
                    label_color_GEFS.setForeground(nc);
                    applyGradientBG(label_color_GEFS);
                }
            }
        });

        container.pack();
        return container;

    }

    private static Image oldImage = null;

    public static void applyGradientBG(Composite c) {

        Rectangle rect = c.getClientArea();
        Image newImage = new Image(c.getDisplay(), rect.width, rect.height);
        GC gc = new GC(newImage);
        gc.setBackground(SWTResourceManager.WHITE);
        gc.fillRectangle(0, 0, rect.width, rect.height);
        gc.setForeground(c.getForeground());
        gc.setBackground(c.getBackground());
        gc.fillGradientRectangle(0, 0, rect.width, rect.height, false);
        c.setBackgroundImage(newImage);
        gc.dispose();

        if (oldImage != null) {
            oldImage.dispose();
            oldImage = newImage;
        }
    }

    /**
     * Create contents of the button bar.
     * 
     * @param parent
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {

        createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL,
                true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Choose Color Range");
    }

}
