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
 * This class is a Dialog which allows users to change the colors of the SREF
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
public class EnsembleSREFColorChooser extends CaveJFACEDialog {

    /**
     * Create the dialog.
     * 
     * @param parentShell
     */
    public EnsembleSREFColorChooser(Shell parentShell) {
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

        Label label_modelName_NMM = new Label(container, SWT.BORDER
                | SWT.CENTER);
        label_modelName_NMM.setAlignment(SWT.CENTER);
        GridData gd_label_modelName_NMM = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 1, 1);
        gd_label_modelName_NMM.heightHint = 20;
        gd_label_modelName_NMM.widthHint = 40;
        label_modelName_NMM.setLayoutData(gd_label_modelName_NMM);
        label_modelName_NMM.setText("NMM");

        Label label_colon_NMM = new Label(container, SWT.NONE);
        label_colon_NMM.setText(":");

        final Composite label_color_NMM = new Composite(container, SWT.BORDER);
        label_color_NMM.setForeground(ChosenSREFColors.getInstance()
                .get_NMM_color());
        label_color_NMM.setBackground(SWTResourceManager.WHITE);
        GridData gd_label_color_NMM = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 3, 1);
        gd_label_color_NMM.heightHint = 24;
        gd_label_color_NMM.widthHint = 116;
        gd_label_color_NMM.minimumWidth = 116;
        gd_label_color_NMM.minimumHeight = 24;
        label_color_NMM.setLayoutData(gd_label_color_NMM);
        label_color_NMM.setSize(116, 24);
        applyGradientBG(label_color_NMM);
        label_color_NMM.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {

                ColorDialog cd = new ColorDialog(getShell());
                cd.setRGB(ChosenSREFColors.getInstance().get_NMM_color()
                        .getRGB());
                cd.setText("Choose SREF-NMM color");
                RGB result = cd.open();
                if (result != null) {
                    Color c = SWTResourceManager.getColor(result);
                    RGB rgb = c.getRGB();
                    float[] hsb = rgb.getHSB();
                    hsb[1] = 1.0f;
                    RGB nrgb = new RGB(hsb[0], hsb[1], hsb[2]);
                    Color nc = SWTResourceManager.getColor(nrgb);
                    ChosenSREFColors.getInstance().set_NMM_color(nc);
                    label_color_NMM.setForeground(nc);
                    applyGradientBG(label_color_NMM);
                }
            }
        });

        Label label_modelName_NMB = new Label(container, SWT.BORDER
                | SWT.CENTER);
        label_modelName_NMB.setAlignment(SWT.CENTER);
        GridData gd_label_modelName_NMB = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 1, 1);
        gd_label_modelName_NMB.heightHint = 20;
        gd_label_modelName_NMB.widthHint = 40;
        label_modelName_NMB.setLayoutData(gd_label_modelName_NMB);
        label_modelName_NMB.setText("NMB");

        Label label_colon_NMB = new Label(container, SWT.NONE);
        label_colon_NMB.setText(":");

        final Composite label_color_NMB = new Composite(container, SWT.BORDER);
        label_color_NMB.setForeground(ChosenSREFColors.getInstance()
                .get_NMB_color());
        label_color_NMB.setBackground(SWTResourceManager.WHITE);
        GridData gd_label_color_NMB = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 3, 1);
        gd_label_color_NMB.heightHint = 24;
        gd_label_color_NMB.widthHint = 116;
        gd_label_color_NMB.minimumWidth = 116;
        gd_label_color_NMB.minimumHeight = 24;
        label_color_NMB.setLayoutData(gd_label_color_NMB);
        label_color_NMB.setSize(116, 24);
        applyGradientBG(label_color_NMB);
        label_color_NMB.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {

                ColorDialog cd = new ColorDialog(getShell());
                cd.setRGB(ChosenSREFColors.getInstance().get_NMB_color()
                        .getRGB());
                cd.setText("Choose SREF-NMB color");
                RGB result = cd.open();
                if (result != null) {
                    Color c = SWTResourceManager.getColor(result);
                    RGB rgb = c.getRGB();
                    float[] hsb = rgb.getHSB();
                    hsb[1] = 1.0f;
                    RGB nrgb = new RGB(hsb[0], hsb[1], hsb[2]);
                    Color nc = SWTResourceManager.getColor(nrgb);
                    ChosenSREFColors.getInstance().set_NMB_color(nc);
                    label_color_NMB.setForeground(nc);
                    applyGradientBG(label_color_NMB);
                }
            }
        });

        Label label_modelName_EM = new Label(container, SWT.BORDER | SWT.CENTER);
        label_modelName_EM.setAlignment(SWT.CENTER);
        GridData gd_label_modelName_EM = new GridData(SWT.CENTER, SWT.CENTER,
                false, false, 1, 1);
        gd_label_modelName_EM.heightHint = 20;
        gd_label_modelName_EM.widthHint = 40;
        label_modelName_EM.setLayoutData(gd_label_modelName_EM);
        label_modelName_EM.setText("EM");

        Label label_colon_EM = new Label(container, SWT.NONE);
        label_colon_EM.setText(":");

        final Composite label_color_EM = new Composite(container, SWT.BORDER);
        label_color_EM.setForeground(ChosenSREFColors.getInstance()
                .get_EM_color());
        label_color_EM.setBackground(SWTResourceManager.WHITE);
        GridData gd_label_color_EM = new GridData(SWT.LEFT, SWT.CENTER, false,
                false, 3, 1);
        gd_label_color_EM.heightHint = 24;
        gd_label_color_EM.widthHint = 116;
        gd_label_color_EM.minimumWidth = 116;
        gd_label_color_EM.minimumHeight = 24;
        label_color_EM.setLayoutData(gd_label_color_EM);
        label_color_EM.setSize(116, 24);
        applyGradientBG(label_color_EM);

        label_color_EM.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {

                ColorDialog cd = new ColorDialog(getShell());
                cd.setRGB(ChosenSREFColors.getInstance().get_EM_color()
                        .getRGB());
                cd.setText("Choose SREF-EM color");
                RGB result = cd.open();
                if (result != null) {
                    Color c = SWTResourceManager.getColor(result);
                    RGB rgb = c.getRGB();
                    float[] hsb = rgb.getHSB();
                    hsb[1] = 1.0f;
                    RGB nrgb = new RGB(hsb[0], hsb[1], hsb[2]);
                    Color nc = SWTResourceManager.getColor(nrgb);
                    ChosenSREFColors.getInstance().set_EM_color(nc);
                    label_color_EM.setForeground(nc);
                    applyGradientBG(label_color_EM);
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
