package bank.online.services;

import bank.online.entities.Credit;

public interface ICreditServices {

	Credit addToCarteBancaire(Credit credit, Long idUser,String carteNumber);
	
	Credit simulerCreditConsommation(Credit credit);
	
	int rembourserCreditParCarteBancaire(Long idEmprunteur,Long idCredit,String numeroCarte,Long idPaiement);
}
