package bank.online.services;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import bank.online.entities.CarteBancaire;
import bank.online.entities.ContratAssurance;
import bank.online.entities.Devises;
import bank.online.entities.InformationComplementaire;
import bank.online.entities.TarifCarteBancaire;
import bank.online.entities.TypeCarteBancaire;
import bank.online.entities.User;
import bank.online.repositories.CarteBancaireRepository;
import bank.online.repositories.ContratAssuranceRepository;
import bank.online.repositories.DevisesRepository;
import bank.online.repositories.InformationComplementaireRepository;
import bank.online.repositories.TarifCarteBancaireRepository;
import bank.online.repositories.TypeCarteBancaireRepository;
import bank.online.repositories.UserRepository;

@Service
public class CarteBancairesServicesImpl implements ICarteBancaireServices{

	Random rand = new Random();
	
	@Autowired
	CarteBancaireRepository carteBRepo;
	
	@Autowired
	DevisesRepository deviseRepo;
	
	@Autowired
	TypeCarteBancaireRepository typeCarteRepo;
	
	@Autowired
	TarifCarteBancaireRepository tarifRepo;
	
	@Autowired
	InformationComplementaireRepository inforRepo;
	
	@Autowired
	ContratAssuranceRepository contratAssuRepo;
	
	@Autowired
	UserRepository userRepository;
	
	
	
	@Override
	public List<CarteBancaire> findAll() {
		
		return carteBRepo.findAll();
	}

	@Transactional
	public CarteBancaire addCarteBancaire(CarteBancaire carte) {
		long diff = 0;
		String numero = String.format((Locale)null, //don't want any thousand separators
		                        "52%02d-%04d-%04d-%04d",
		                        rand.nextInt(100),
		                        rand.nextInt(10000),
		                        rand.nextInt(10000),
		                        rand.nextInt(10000));
		carte.setNumero(numero);
         Date toDay = new Date();
         Date newDate = new Date();
         
         SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
         Calendar cal = Calendar.getInstance(); 
 		 cal.add(Calendar.YEAR, 3);
		//Date après avoir ajouté les années à la date indiquée
		String echeance = sdf.format(cal.getTime());
		try {
			newDate = sdf.parse(echeance);
		} catch (ParseException e) {
			e.printStackTrace();
		}
		  
		carte.setEcheance(newDate);
		diff = newDate.getTime() - toDay.getTime();
		
		TimeUnit time = TimeUnit.DAYS; 
	    carte.setDureeContractuelle(time.convert(diff, TimeUnit.MILLISECONDS)); //calculer la durée contractuelle
	    carte.setDateCreation(new Date());
	    carte.getContratAssurance().setDateCreation(new Date());
	    carte.getTypeCarte().setDateCreation(new Date());
	    
	    TypeCarteBancaire typeCarte = carte.getTypeCarte();
	    for (InformationComplementaire detail : typeCarte.getDetails()) {
			detail.setDateCreation(new Date());
			inforRepo.save(detail);
		}
	    
	  
	    typeCarte.getTarif().setDateCreation(new Date());
	    contratAssuRepo.save(carte.getContratAssurance()); //enregistrer le contrat d'assurance
	    TarifCarteBancaire tarif = typeCarte.getTarif();
	    Devises devise = typeCarte.getTarif().getDevise();
	    tarif.setDevise(devise);
	    tarif.setDateCreation(new Date());
	    deviseRepo.save(devise);
	    tarifRepo.save(typeCarte.getTarif());
	    typeCarteRepo.save(typeCarte); //enregistrer le type de la carte
	    
		return carteBRepo.save(carte);
	}

	@Override
	public Integer souscrieCarteBancaire(Long idCarte, Long idUser) {
		Optional<User> foundUser = userRepository.findById(idUser);
		Optional<CarteBancaire> foundCard = carteBRepo.findById(idCarte);
		if(foundUser.isPresent() && foundCard.isPresent()) {
			User user = foundUser.get();
			CarteBancaire card = foundCard.get();
			Optional<CarteBancaire> verifCard = carteBRepo.getIfSubscribedCard(card.getIdCarteB());
			
			if(verifCard.isPresent()) {
				return 2;
			}
			
			if(Boolean.FALSE.equals(user.getCarteBancaires().contains(card))) {
				card.setEstSoucrite(true);
				card.setDateModification(new Date());
				carteBRepo.save(card);
				user.getCarteBancaires().add(card);
				userRepository.save(user);
				return 0;
			}else {
				return 1;
			}
			
		}
		return -1;
	}

	@Override
	public List<CarteBancaire> getCarteByUser(Long idUser) {
		Optional<User> foundUser = userRepository.findById(idUser);
		if(foundUser.isPresent()) {
			return carteBRepo.getCardsByUser(foundUser.get().getId());
		}
		return Collections.emptyList();
	}

	@Override
	public CarteBancaire getCarteBancaire(Long idCarte) {
		return carteBRepo.findById(idCarte).orElse(null);
	}

	@Transactional
	public CarteBancaire editCarteBancaire(CarteBancaire carte) {
		Optional<CarteBancaire> foundCard = carteBRepo.findById(carte.getIdCarteB());
		String cardNumber ="";
		if(foundCard.isPresent()) {
			CarteBancaire card = foundCard.get();
			cardNumber = card.getNumero();
		}
		carte.setDateModification(new Date());
		carte.setNumero(cardNumber);
		
		carte.getContratAssurance().setDateModification(new Date());
		contratAssuRepo.save(carte.getContratAssurance()); //enregistrer le contrat d'assurance
		for (InformationComplementaire detail : carte.getTypeCarte().getDetails()) {
			detail.setDateModification(new Date());
			inforRepo.save(detail);
		}
		carte.getTypeCarte().getTarif().setDateModification(new Date());
	    tarifRepo.save(carte.getTypeCarte().getTarif());
	    carte.getTypeCarte().setDateModification(new Date());
	    typeCarteRepo.save(carte.getTypeCarte()); //enregistrer le type de la carte
	    
		return carteBRepo.save(carte);
	}

	@Override
	public List<CarteBancaire> getCarteByType(String typeName) {
		
		return carteBRepo.getCardsByType(typeName);
	}

	@Override
	public void removeCarte(Long idCarte) {
		Optional<CarteBancaire> gottenCarte = carteBRepo.findById(idCarte);
		
		if(gottenCarte.isPresent()) {
			typeCarteRepo.delete(gottenCarte.get().getTypeCarte());
			
			gottenCarte.get().getContratAssurance().setEstResilie(true);
			contratAssuRepo.save(gottenCarte.get().getContratAssurance());
			
			List<User> possedantsDeLaCartes = userRepository.getUsersByCards(gottenCarte.get().getIdCarteB());
			for (User user : possedantsDeLaCartes) {
				user.getCarteBancaires().remove(gottenCarte.get());
				userRepository.save(user);
			}
			carteBRepo.delete(gottenCarte.get());
		}
	}

	@Override
	public Integer cancelSubscriptionCarteBancaire(Long idCarte, Long idUser) {
		Optional<User> foundUser = userRepository.findById(idUser);
		Optional<CarteBancaire> foundCard = carteBRepo.findById(idCarte);
		if(foundUser.isPresent() && foundCard.isPresent()) {
			User user = foundUser.get();
			CarteBancaire card = foundCard.get();
			card.setEstSoucrite(false);
			user.getCarteBancaires().remove(card);
			carteBRepo.save(card);
			userRepository.save(user);
			return 0;
		}
		return -1;
	}

	@Override
	public Integer resilierContratSubsciption(Long idCarte, Long idUser) {
		Optional<User> foundUser = userRepository.findById(idUser);
		Optional<CarteBancaire> foundCard = carteBRepo.findById(idCarte);
		if(foundUser.isPresent() && foundCard.isPresent()) {
			User user = foundUser.get();
			CarteBancaire card = foundCard.get();
			
			if(user.getCarteBancaires().contains(card)) {
				card.getContratAssurance().setEstResilie(true);
				card.setDateModification(new Date());
				card.getContratAssurance().setDateModification(new Date());
				card.getContratAssurance().setDateResiliation(new Date());
				contratAssuRepo.save(card.getContratAssurance());
				card.setContratAssurance(null);
				carteBRepo.save(card);
				return 0;
			}
			else {
				return 1;
			}	
		}
		return -1;
	}

	@Override
	public Integer ajouterUnContratAssurance(Long idCarte, Long idContrat, Long idUser) {
		Optional<User> foundUser = userRepository.findById(idUser);
		Optional<CarteBancaire> foundCard = carteBRepo.findById(idCarte);
		Optional<ContratAssurance> foundContrat = contratAssuRepo.findById(idContrat);
		
		if(foundUser.isPresent() && foundCard.isPresent() && foundContrat.isPresent()) {
			User user = foundUser.get();
			CarteBancaire card = foundCard.get();
			ContratAssurance contrat = foundContrat.get();
			if(user.getCarteBancaires().contains(card)) {
				card.setContratAssurance(contrat);
				card.setDateModification(new Date());
				carteBRepo.save(card);
				return 0;
			}else {
				return 1;
			}	
		}
		return -1;
	}

	@Override
	public Integer transfertFromCardToCard(Long idUser,String cardDeb, String cardCred,float montant) {
		Optional<User> userOptional = userRepository.findById(idUser);
		
		if(!userOptional.isPresent()) {
			return 1;//l'utilisateur n'existe pas!
		}
		
		Optional<CarteBancaire> cardDebOptional = carteBRepo.getCardByNumber(cardDeb, idUser);
		Optional<CarteBancaire> cardCredOptional = carteBRepo.getCardByNumber(cardCred, idUser);
		
		if(cardDebOptional.isPresent() && cardCredOptional.isPresent() && cardDebOptional.get() != cardCredOptional.get()) {
			CarteBancaire carteDeb = cardDebOptional.get();
			CarteBancaire carteCred = cardCredOptional.get();
			
			if(carteDeb.getTypeCarte().getTarif().getProvision() >= montant) {
				TarifCarteBancaire tarifDeb = carteDeb.getTypeCarte().getTarif();
				tarifDeb.setProvision(tarifDeb.getProvision() - montant);
				tarifDeb.setDateModification(new Date());
				tarifRepo.save(tarifDeb);
				
				TarifCarteBancaire tarifCred = carteCred.getTypeCarte().getTarif();
				tarifCred.setProvision(tarifCred.getProvision() + montant);
				tarifCred.setDateModification(new Date());
				tarifRepo.save(tarifCred);
				
				return 0; //L'opération a reussie!
			}else {
				return 3;//vous avez un manque de provision sur la carte à debiter
			}
		}else {
			return 2; //au moins une carte est introuvable!
		}
	}
}