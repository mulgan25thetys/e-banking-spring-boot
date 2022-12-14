package bank.online.services;

import javax.mail.MessagingException;

import bank.online.entities.Credit;
import bank.online.entities.User;

public interface INotificationServices {

	public void sendMailWithCode(User user,Boolean withCode) throws MessagingException;
	
	public void notifiyPersonnale(User user,String password) throws MessagingException;
	
	public void notifiyForcreditConfirmation(Credit credit,User user) throws MessagingException;
}
