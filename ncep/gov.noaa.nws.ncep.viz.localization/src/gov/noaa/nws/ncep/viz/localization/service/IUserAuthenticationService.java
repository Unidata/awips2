package gov.noaa.nws.ncep.viz.localization.service;

public interface IUserAuthenticationService {
	boolean isUserValid(String userName, String password); 
}
