package bank.online.services;

import java.text.DateFormat;
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
import bank.online.entities.InformationComplementaire;
import bank.online.entities.TypeCarteBancaire;
import bank.online.entities.User;
import bank.online.repositories.CarteBancaireRepository;
import bank.online.repositories.ContratAssuranceRepository;
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

	@Override
	public CarteBancaire addCarteBancaire(CarteBancaire carte) {
		long diff = 0;
		String numero = String.format((Locale)null, //don't want any thousand separators
		                        "52%02d-%04d-%04d-%04d",
		                        rand.nextInt(100),
		                        rand.nextInt(10000),
		                        rand.nextInt(10000),
		                        rand.nextInt(10000));
		carte.setNumero(numero);
		 Date date = Calendar.getInstance().getTime();  
         DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");  
         String toDayFormat =  dateFormat.format(date);  
         Date toDay = new Date();
		
         SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
		  Calendar cal = Calendar.getInstance();
		  try{
		     //Définir la date
		     cal.setTime(sdf.parse(toDayFormat));
		  }catch(ParseException e){
		    e.printStackTrace();
		   }
		  cal.add(Calendar.DAY_OF_YEAR, 3);  
		  //Date après avoir ajouté les années à la date indiquée
		  String echeance = sdf.format(cal.getTime()); 
		  
		  try {
			carte.setEcheance(sdf.parse(echeance));
			diff = sdf.parse(echeance).getTime() - toDay.getTime();
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
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
}