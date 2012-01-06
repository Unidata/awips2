package ohd.hseb.mpe.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import ohd.hseb.grid.XmrgGrid;
import ohd.hseb.util.AppsDefaults;

/**
 * 
 * @author lawrence
 * 
 * Represents an MPE Product.
 * 
 */
public class MPEProductDescriptor implements Comparable<MPEProductDescriptor>
{
    public static final String MPE_RMOSAIC = "RMOSAIC";
    public static final String MPE_AVGRMOSAIC = "AVGRMOSAIC";
    public static final String MPE_MAXRMOSAIC = "MAXRMOSAIC";
    public static final String MPE_BMOSAIC = "BMOSAIC";
    public static final String MPE_LMOSAIC = "LMOSAIC";
    public static final String MPE_GAGEONLY = "GAGEONLY";
    public static final String MPE_MMOSAIC = "MMOSAIC";
    public static final String MPE_MLMOSAIC = "MLMOSAIC";
    public static final String MPE_SATPRE = "SATPRE";
    public static final String MPE_LSATPRE = "LSATPRE";
    public static final String MPE_SRMOSAIC = "SRMOSAIC";
    public static final String MPE_SGMOSAIC = "SGMOSAIC";
    public static final String MPE_SRGMOSAIC = "SRGMOSAIC";
    public static final String MPE_P3LMOSAIC = "P3LMOSAIC";
    public static final String MPE_BESTQPE = "xmrg";
    public static final String MPE_RFCBMOSAIC = "RFCBMOSAIC";
    public static final String MPE_RFCMMOSAIC = "RFCMMOSAIC";
    public static final String MPE_RFCMOSAIC = "RFCMOSAIC";
    
    //Q2 products
    public static final String MPE_QMOSAIC = "QMOSAIC";
    public static final String MPE_LQMOSAIC = "LQMOSAIC";
    public static final String MPE_MLQMOSAIC = "MLQMOSAIC";
 
    

    public static final MPEProductDescriptor MPE_RMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Radar Mosaic", MPE_RMOSAIC, "Radar Mosaic", "mpe_rmosaic_dir",
            null, null);

    public static final MPEProductDescriptor MPE_AVGRMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Average Radar Mosaic", MPE_AVGRMOSAIC, "Average Radar Mosaic",
            "mpe_avgrmosaic_dir", null, null);

    public static final MPEProductDescriptor MPE_MAXRMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Maximum Radar Mosaic", MPE_MAXRMOSAIC, "Maximum Radar Mosaic",
            "mpe_maxrmosaic_dir", null, null);

    public static final MPEProductDescriptor MPE_BMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Field Bias Mosaic", MPE_BMOSAIC, "Field Bias Mosaic",
            "mpe_bmosaic_dir", new String[] { MPE_RMOSAIC }, null);

    public static final MPEProductDescriptor MPE_LMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Local Bias Mosaic", MPE_LMOSAIC, "Local Bias Mosaic",
            "mpe_lmosaic_dir", new String[] { MPE_RMOSAIC }, null);

    public static final MPEProductDescriptor MPE_GAGEONLY_DESCRIPTOR = new MPEProductDescriptor(
            "Gage Only Analysis", MPE_GAGEONLY, "Gage Only Analysis",
            "mpe_gageonly_dir", new String[] { MPE_RMOSAIC }, null);

    public static final MPEProductDescriptor MPE_MMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Multi-sensor Mosaic", MPE_MMOSAIC, "Multi-sensor Mosaic",
            "mpe_mmosaic_dir", new String[] { MPE_RMOSAIC,
                    "MMOSAIC" }, null);

    public static final MPEProductDescriptor MPE_MLMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Local Bias Multi-sensor Mosaic", MPE_MLMOSAIC, "Local Bias Multi-sensor Mosaic",
            "mpe_mlmosaic_dir", new String[] { MPE_RMOSAIC, MPE_LMOSAIC }, null);

    public static final MPEProductDescriptor MPE_SATPRE_DESCRIPTOR = new MPEProductDescriptor(
            "Satellite Precipitation Field", MPE_SATPRE, "Satellite Precipitation Field",
            "mpe_satpre_dir", null, null);

    public static final MPEProductDescriptor MPE_LSATPRE_DESCRIPTOR = new MPEProductDescriptor(
            "Local Bias Satellite Precipitation Field",
            MPE_LSATPRE,
            "Local Bias Satellite Precipitation Field", "mpe_lsatpre_dir",
            new String[] { MPE_RMOSAIC,
                    MPE_LMOSAIC }, null);

    public static final MPEProductDescriptor MPE_SRMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Satellite Radar Mosaic", MPE_SRMOSAIC, "Satellite Radar Mosaic",
            "mpe_srmosaic_dir", new String[] { MPE_LSATPRE,
                    MPE_LMOSAIC }, null);

    public static final MPEProductDescriptor MPE_SGMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Satellite Gage Mosaic", MPE_SGMOSAIC, "Satellite Gage Mosaic",
            "mpe_sgmosaic_dir", new String[] {MPE_LSATPRE }, null);

    public static final MPEProductDescriptor MPE_SRGMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Satellite Radar Gage Mosaic", MPE_SRGMOSAIC, "Satellite Radar Gage Mosaic",
            "mpe_srgmosaic_dir", new String[] { MPE_LSATPRE, MPE_LMOSAIC }, null);

    public static final MPEProductDescriptor MPE_P3LMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "Triangulated Radar Mosaic", MPE_P3LMOSAIC, "Triangulated Radar Mosaic",
            "mpe_p3lmosaic_dir", new String[] { MPE_RMOSAIC }, null);
    
    public static final MPEProductDescriptor MPE_BESTQPE_DESCRIPTOR = new MPEProductDescriptor(
            "Best Estimate QPE", MPE_BESTQPE, "Best Estimate QPE", "mpe_qpe_dir", null, null);

    public static final MPEProductDescriptor MPE_RFCBMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "RFC Mean Field Bias Mosaic", MPE_RFCBMOSAIC, "RFC Mean Field Bias Mosaic",
            "mpe_rfcbmosaic_dir", new String[] { MPE_RMOSAIC }, null);

    public static final MPEProductDescriptor MPE_RFCMMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "RFC Multi-sensor Mosaic", MPE_RFCMMOSAIC, "RFC Multi-sensor Mosaic",
            "mpe_rfcmmosaic_dir", new String[] { MPE_RMOSAIC, MPE_RFCBMOSAIC }, null);

    public static final MPEProductDescriptor MPE_RFCMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
            "RFC Best QPE Mosaic", MPE_RFCMOSAIC, "RFC Best QPE Mosaic",
            "mpe_rfcmosaic_dir", null, null);

//new Q2 products
   
   public static final MPEProductDescriptor MPE_QMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
           "Q2 Mosaic", MPE_QMOSAIC, "Q2 Mosaic",
           "mpe_qmosaic_dir", null, null);

   public static final MPEProductDescriptor MPE_LQMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
           "Q2 Local Bias Mosaic", MPE_LQMOSAIC, "Q2 Local Bias Mosaic",
           "mpe_lqmosaic_dir", new String[] { MPE_QMOSAIC }, null);

   public static final MPEProductDescriptor MPE_MLQMOSAIC_DESCRIPTOR = new MPEProductDescriptor(
           "Q2 Multi-sensor Mosaic", MPE_MLQMOSAIC, "Q2 Multi-sensor Mosaic",
           "mpe_mlqmosaic_dir", new String[] { MPE_QMOSAIC, MPE_LQMOSAIC }, null);



   private static List<MPEProductDescriptor> _listOfAllPossibleMPEProductDescriptors = new ArrayList<MPEProductDescriptor>();;
   private static Map<String, MPEProductDescriptor> _mapOfAllPossibleMPEProductDescriptors = new HashMap <String, MPEProductDescriptor>();
   private static List<MPEProductDescriptor> _listOfAvailableMPEProductDescriptors = new ArrayList<MPEProductDescriptor>();

    static
    {
        
     //   _listOfAllPossibleMPEProductDescriptors = new ArrayList<MPEProductDescriptor>();
       
        // This was commented out by Chip Gobs on 10/1/08 
        // The code was reduced in size by using the private constructor to do this work.
         
        addDescriptorToListAndHashMap(MPE_RMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_AVGRMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_MAXRMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_BMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_LMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_GAGEONLY_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_MMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_MLMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_SATPRE_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_LSATPRE_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_SRMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_SGMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_SRGMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_P3LMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_BESTQPE_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_RFCBMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_RFCMMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_RFCMOSAIC_DESCRIPTOR);
        
        
        // Q2 products
        addDescriptorToListAndHashMap(MPE_QMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_LQMOSAIC_DESCRIPTOR);
        addDescriptorToListAndHashMap(MPE_MLQMOSAIC_DESCRIPTOR);
     
     
        
        	/* Populate lists of available MPE Products, those are being 
         * created by MPEFieldGen.
         */
      
        determineAvailableMPEProducts();
        
        // The list of available MPE products is initially ordered by the how they are
        // specified in the mpe_generate_list token.  Reorder the products to match
        // how they appear on the PrecipFields menu.
        orderAvailableMPEProductsToMatchPrecipFieldsMenu();
    }

    private String _productName;
    private String _productFilenamePrefix;
    private String _productDescription;
    private String _productPathToken;
    private String[] _dependencies;
    private XmrgGrid _mpeProductDataGrid;

    private MPEProductDescriptor(String productName, String productFilenamePrefix, String productDescription,
            String productPathToken, String[] dependencies, XmrgGrid mpeProductDataGrid)
    {
        setProductName(productName);
        setProductFilenamePrefix(productFilenamePrefix);
        setProductDescription(productDescription);
        setProductPathToken(productPathToken);
        setDependencies(dependencies);
        setMpeProductDataGrid(mpeProductDataGrid);
        
    //    addDescriptorToListAndHashMap(this);
    }
    
    private static void  addDescriptorToListAndHashMap(MPEProductDescriptor descriptor)
    {
        _listOfAllPossibleMPEProductDescriptors.add(descriptor);
        _mapOfAllPossibleMPEProductDescriptors.put(descriptor.getProductFilenamePrefix(), descriptor);
    }
    
    public static List<MPEProductDescriptor> getListOfAllPossibleMPEProductDescriptors()
    {
        return _listOfAllPossibleMPEProductDescriptors;
    }
    
    public static List<MPEProductDescriptor> getListOfAllAvailableMPEProductDescriptors()
    {
        return _listOfAvailableMPEProductDescriptors;
    }
    
    private static Map<String, MPEProductDescriptor> getMapOfAllPossibleMPEProductDescriptors()
    {
        return _mapOfAllPossibleMPEProductDescriptors;
    }
    
    private static void determineAvailableMPEProducts ()
    {
        getAvailableMpeProductsFromMPEGenerateList();
        addMPEQPEFieldTypeIfNeeded();
        addRFCMosaicIfNeeded();
        addBaseRadarMosaicIfNeeded();
    }
    
    private static void orderAvailableMPEProductsToMatchPrecipFieldsMenu ()
    {
        // Iterate over the list of all possible MPE Products.  It is
        // assumed that this list is in the same order as the items
        // on the MPE Editor PrecipFields menu.
    	
        MPEProductDescriptor product;
        List<MPEProductDescriptor> _tempListOfAvailableMPEProductDescriptors;
        _tempListOfAvailableMPEProductDescriptors = new ArrayList <MPEProductDescriptor>();
        Iterator <MPEProductDescriptor> MPEproduct = _listOfAllPossibleMPEProductDescriptors.iterator();

        while ( MPEproduct.hasNext() )
        {
            product = MPEproduct.next();

            if ( _listOfAvailableMPEProductDescriptors.contains(product))
            {
                _tempListOfAvailableMPEProductDescriptors.add(product);
            }
        }

        _listOfAvailableMPEProductDescriptors = _tempListOfAvailableMPEProductDescriptors;

    }
    
    private static void getAvailableMpeProductsFromMPEGenerateList()
    {
        String header = "MPEProductDescriptor.getAvailableMpeProductsFromMPEGenerateList(): ";
        
        final String MPE_GENERATE_LIST_TOKEN = "mpe_generate_list";
        final String DEFAULT_GENERATE_LIST = MPE_BMOSAIC + "," + MPE_GAGEONLY + "," +
                                             MPE_LMOSAIC + "," + MPE_LSATPRE  + "," +
                                             MPE_MLMOSAIC + "," + MPE_MMOSAIC + "," +
                                             MPE_RMOSAIC + "," + MPE_SATPRE;
        
        AppsDefaults appsDefaults = new AppsDefaults();
        MPEProductDescriptor mpeProductDescriptor;
        String generateList;
        String token;
        StringTokenizer generateListTokens;

        generateList = appsDefaults.getToken(MPE_GENERATE_LIST_TOKEN,
                DEFAULT_GENERATE_LIST);
        
        generateListTokens = new StringTokenizer(generateList, ",");

         
        while (generateListTokens.hasMoreElements())
        {
            token = generateListTokens.nextToken();
            
            mpeProductDescriptor = getMapOfAllPossibleMPEProductDescriptors().get(token);
            
            if (mpeProductDescriptor != null )
            {
                addMPEProductToGenerateListIfNeeded(mpeProductDescriptor);
                System.out.println(header + "mpeProduct = " + mpeProductDescriptor.getProductFilenamePrefix());
            }
            else
            {
                System.out.println(header + "Unrecognized mpe_generate_list value "
                        + token + ".");
            }
            
        }
        
        // Force the addition of the XMRG field.  The XMRG field is always 
        // generated.
        mpeProductDescriptor = getMapOfAllPossibleMPEProductDescriptors().get(MPE_BESTQPE);
        addMPEProductToGenerateListIfNeeded(mpeProductDescriptor);
    
        
    }
    
    private static void addMPEQPEFieldTypeIfNeeded()
    {
        final String MPE_QPE_FIELDTYPE_TOKEN = "mpe_qpe_fieldtype";
        final String DEFAULT_QPE_FIELD_TYPE = MPE_MMOSAIC;
        
        AppsDefaults appsDefaults = new AppsDefaults();
        String bestQPE = appsDefaults.getToken(MPE_QPE_FIELDTYPE_TOKEN,
                DEFAULT_QPE_FIELD_TYPE);

        System.out.println ("The value of token " + MPE_QPE_FIELDTYPE_TOKEN + " is " + bestQPE );
        
        MPEProductDescriptor mpeProduct = getMapOfAllPossibleMPEProductDescriptors().get(bestQPE);

        if (mpeProduct == null)
        {
            System.out.println("Invalid mpe_qpe_fieldtype variable: " + bestQPE);
            System.out.println("Using default mpe_qpe_fieldtype value: "
                    + DEFAULT_QPE_FIELD_TYPE);
            mpeProduct = getMapOfAllPossibleMPEProductDescriptors().get(DEFAULT_QPE_FIELD_TYPE);
        }

        addMPEProductToGenerateListIfNeeded(mpeProduct);
    }
    
    private static void addRFCMosaicIfNeeded()
    {
        final String GENERATE_AREAL_QPE_TOKEN = "mpe_generate_areal_qpe";
        final String DEFAULT_GENERATE_AREAL_QPE = "OFF";
        
        // The RFC Mosaic is handled in a special way.
        AppsDefaults appsDefaults = new AppsDefaults();
        MPEProductDescriptor mpeProduct;
        
        String generateArealQPE = appsDefaults.getToken(
                GENERATE_AREAL_QPE_TOKEN, DEFAULT_GENERATE_AREAL_QPE);

        System.out.println ( "The value of token " + GENERATE_AREAL_QPE_TOKEN + " is " + generateArealQPE);
        
        if (generateArealQPE.compareToIgnoreCase("ON") == 0)
        {
            mpeProduct = getMapOfAllPossibleMPEProductDescriptors().get(MPE_RFCMOSAIC);
            
            if ( mpeProduct!= null)
            {
                if ( !getListOfAllAvailableMPEProductDescriptors().contains(mpeProduct))
                {
                   getListOfAvailableMPEProductDescriptors().add(mpeProduct);
                   checkProductDependencies(mpeProduct);
                }
            }
        }
    }
    
    private static void addBaseRadarMosaicIfNeeded()
    {
        final String MPE_BASE_RADAR_MOSAIC_TOKEN = "mpe_base_radar_mosaic";
        final String DEFAULT_BASE_RADAR_MOSAIC = "RMOSAIC";
        
     // The RFC Mosaic is handled in a special way.
        AppsDefaults appsDefaults = new AppsDefaults();
        MPEProductDescriptor mpeProduct;
        
        String mpeBaseRadarMosaic = appsDefaults.getToken(MPE_BASE_RADAR_MOSAIC_TOKEN, DEFAULT_BASE_RADAR_MOSAIC);
        System.out.println ( "The value of token " + MPE_BASE_RADAR_MOSAIC_TOKEN + " is " + mpeBaseRadarMosaic);
        
        mpeProduct = getMapOfAllPossibleMPEProductDescriptors().get(mpeBaseRadarMosaic);
        
        if ( mpeProduct!= null)
        {
            if ( !getListOfAllAvailableMPEProductDescriptors().contains(mpeProduct))
            {
               getListOfAvailableMPEProductDescriptors().add(mpeProduct);
               checkProductDependencies(mpeProduct);
            }
        }

    }

    private static void checkProductDependencies(MPEProductDescriptor product)
    {
        String dependency;
        MPEProductDescriptor dependentMPEProductDescriptor;

        if (product.getDependencies() != null)
        {
            for (int i = 0; i < product.getDependencies().length; ++i)
            {
                dependency = product.getDependencies()[i];
                dependentMPEProductDescriptor = getMapOfAllPossibleMPEProductDescriptors().get(
                        dependency);

                if (dependentMPEProductDescriptor != null)
                {
                    if (!getListOfAvailableMPEProductDescriptors().contains(
                            dependentMPEProductDescriptor))
                    {
                        getListOfAvailableMPEProductDescriptors().add(dependentMPEProductDescriptor);
                        checkProductDependencies(dependentMPEProductDescriptor);
                    }
                }
                else
                {
                    System.out.println("For product "
                            + product.getProductName()
                            + " unrecognized dependency " + dependency);
                }

            }
        }
    }
     
    static private void addMPEProductToGenerateListIfNeeded( MPEProductDescriptor productDescriptor)
    {
        if (productDescriptor != null)
        {
            if (!getListOfAvailableMPEProductDescriptors().contains(
                    productDescriptor))
            {
                getListOfAvailableMPEProductDescriptors().add(productDescriptor);
                checkProductDependencies(productDescriptor);
            }
        }

    }
    
    public void setProductName(String productName)
    {
        _productName = productName;
    }

    public String getProductName()
    {
        return _productName;
    }

    public void setProductDescription(String productDescription)
    {
        _productDescription = productDescription;
    }

    public String getProductDescription()
    {
        return _productDescription;
    }

    public void setProductPathToken(String productPathToken)
    {
        _productPathToken = productPathToken;
    }

    public String getProductPathToken()
    {
        return _productPathToken;
    }

    public void setDependencies(String[] dependencies)
    {
        _dependencies = dependencies;
    }

    public String[] getDependencies()
    {
        return _dependencies;
    }

    public int compareTo(MPEProductDescriptor o)
    {
        String thisProductName = getProductName();
        String thatProductName = o.getProductName();

        // TODO Auto-generated method stub
        return thisProductName.compareTo(thatProductName);
    }

    public void setMpeProductDataGrid(XmrgGrid mpeProductDataGrid)
    {
        this._mpeProductDataGrid = mpeProductDataGrid;
    }

    public XmrgGrid getMpeProductDataGrid()
    {
        return _mpeProductDataGrid;
    }

    public static void setListOfAvailableMPEProductDescriptors(
            List<MPEProductDescriptor> listOfAvailableMPEProductDescriptors)
    {
        MPEProductDescriptor._listOfAvailableMPEProductDescriptors = listOfAvailableMPEProductDescriptors;
    }

    public static List<MPEProductDescriptor> getListOfAvailableMPEProductDescriptors()
    {
        return _listOfAvailableMPEProductDescriptors;
    }

    public void setProductFilenamePrefix(String productFilenamePrefix)
    {
        _productFilenamePrefix = productFilenamePrefix;
    }

    public String getProductFilenamePrefix()
    {
        return _productFilenamePrefix;
    }

}
