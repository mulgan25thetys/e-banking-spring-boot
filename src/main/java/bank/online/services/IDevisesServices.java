package bank.online.services;

import java.util.List;

import bank.online.entities.Devises;

public interface IDevisesServices {

	List<Devises> findAll();
	
	Devises addDevise(Devises devise);
	
	Devises editDevise(Devises devise);
	
	Devises getDevise(Long id);
	
	void removeDevise(Long id);
}
