package gov.noaa.nws.ncep.viz.localization.admin;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.AbstractSourceProvider;
import org.eclipse.ui.ISources;

public class AdminLoginStateSourceProvider extends AbstractSourceProvider {
	public final static String ADMIN_LOGIN_STATE = "gov.noaa.nws.ncep.viz.localization.admin.adminLoginState"; 
	private final static String ADMIN_LOGGED_IN = "loggedIn";
	private final static String ADMIN_LOGGED_OUT = "loggedOut"; 
	private static boolean isLoggedIn; 
	
	public AdminLoginStateSourceProvider() {
	}

	@Override
	public void dispose() {
	}

	@Override
	public String[] getProvidedSourceNames() {
		return new String[] {ADMIN_LOGIN_STATE}; 
	}

	@Override
	public Map<String, String> getCurrentState() {
		Map<String, String> currentStateMap = new HashMap<String, String>(1);
        String currentState =  isLoggedIn ? ADMIN_LOGGED_IN:ADMIN_LOGGED_OUT;
        currentStateMap.put(ADMIN_LOGIN_STATE, currentState);
        return currentStateMap; 
	}

	public void setAdminLoggedInState(boolean _isLoggedIn) {
		if(AdminLoginStateSourceProvider.isLoggedIn == _isLoggedIn)
			return; // no change
		AdminLoginStateSourceProvider.isLoggedIn = _isLoggedIn; 
		String currentState =  AdminLoginStateSourceProvider.isLoggedIn ? ADMIN_LOGGED_IN:ADMIN_LOGGED_OUT;
		fireSourceChanged(ISources.WORKBENCH, ADMIN_LOGIN_STATE, currentState);
	} 
	
	public static boolean isAdminUserLoggedIn() {
		return isLoggedIn; 
	}
}
