package gov.noaa.nws.ncep.viz.localization.service;

import gov.noaa.nws.ncep.viz.localization.model.AdminUser;

import java.util.ArrayList;
import java.util.List;

public class UserAuthenticationServiceImpl implements
		IUserAuthenticationService {

	private List<AdminUser> userList; 
	
	public UserAuthenticationServiceImpl() {
		userList = new ArrayList<AdminUser>(10); 
		initUserList(); 
	}
	
	@Override
	public boolean isUserValid(String userName, String password) {
		boolean isValid = validateUser(userName, password); 
		return isValid;
	}

	private boolean validateUser(String userName, String password) {
		boolean isUserValid = false; 
		AdminUser inputUser = getUserByUserNameAndPassword(userName, password); 
		
		for(AdminUser eachUser : userList) {
			if(eachUser.equals(inputUser)) {
				isUserValid = true; 
				break; 
			}
		}
		return isUserValid; 
	}
	
	private AdminUser getUserByUserNameAndPassword(String userName, String password) {
		AdminUser newUser = new AdminUser(); 
		newUser.setUserName(userName); 
		newUser.setPassword(password); 
		return newUser; 
	}

	private void initUserList() {
		AdminUser tempUser = getUserByUserNameAndPassword("steve", "1357"); 
		userList.add(tempUser); 
		
		tempUser = getUserByUserNameAndPassword("scott", "tiger"); 
		userList.add(tempUser); 

		tempUser = getUserByUserNameAndPassword("dave", "123"); 
		userList.add(tempUser); 
	
	}
}
