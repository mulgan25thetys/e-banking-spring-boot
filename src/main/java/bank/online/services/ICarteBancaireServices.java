package bank.online.services;

import java.util.List;

import bank.online.entities.CarteBancaire;

public interface ICarteBancaireServices {

	List<CarteBancaire> findAll();
	
	CarteBancaire addCarteBancaire(CarteBancaire carte);
	
	Integer souscrieCarteBancaire(Long idCarte,Long idUser);
	
	List<CarteBancaire> getCarteByUser(Long idUser);
	
	CarteBancaire getCarteBancaire(Long idCarte);
	
	CarteBancaire editCarteBancaire(CarteBancaire carte);
	
	List<CarteBancaire> getCarteByType(String typeName);
	
	void removeCarte(Long idCarte);
	
	Integer cancelSubscriptionCarteBancaire(Long idCarte,Long idUser);
	
	Integer resilierContratSubsciption(Long idCarte,Long idUser);
	
	Integer ajouterUnContratAssurance(Long idCarte,Long idContrat,Long idUser);
	
	Integer transfertFromCardToCard(Long idUser,String cardDeb,String cardCred,float montant);
}
