package bank.online.services;

import org.springframework.core.io.Resource;
import org.springframework.web.multipart.MultipartFile;

import bank.online.entities.Credit;

public interface ICreditServices {

	Credit addToCarteBancaire(Credit credit, Long idUser,String carteNumber);
	
	Credit simulerCreditConsommation(Credit credit);
	
	Credit simulerCreditImmobilier(Credit credit,Long idUser);
	
	int rembourserCreditParCarteBancaire(Long idEmprunteur,Long idCredit,String numeroCarte,Long idPaiement);
	
	int accorderCreditImmo(Long idCredit);
	
	public void save(MultipartFile file,Long idCredit);
	
	public Resource load(String filename);
	
	void useCredit(Credit credit, Long idUser,String carteNumber);
}
