package bank.online.services;

import java.util.List;

import bank.online.entities.ReseauPaiement;

public interface IReseauPaiementServices {

	List<ReseauPaiement> findAll();
	
	ReseauPaiement addReseauPay(ReseauPaiement reseau);
	
	ReseauPaiement editReseauPay(ReseauPaiement reseau);
	
	ReseauPaiement getReseauPaiement(Long id);
	
	void deleteReseauPay(Long id);
}
