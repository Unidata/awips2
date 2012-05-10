package gov.noaa.nws.ncep.standalone.xmlConverter;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

/**
 * XmlConvert
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/25/2009   137         Q. Zhou     Initial created
 * 
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class XmlConvertHandler extends AbstractHandler { //implements IHandler {
	
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

	/*	ISingleResourceFileInfoLoader<File> singleResourceLoader = new SingleFileLoaderImpl();
		final ILoadedFileResourceInfo<File> loadedFileInfoObject = getLoadedFileInfoObject("necp", 
				"base/stns/", "resourceLocalizationLevelStringValue", "spcwatch.xml"); 
		
		File countyLadedFile = loadedFileInfoObject.doFileInfoLoading(singleResourceLoader);
	*/	
		XmlConvertDialog cd = new XmlConvertDialog(HandlerUtil.getActiveWorkbenchWindow(event).getShell());
		cd.open();
		
		return null;
	}
	
}


