package bank.online.services;

import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import javax.mail.MessagingException;
import javax.transaction.Transactional;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import bank.online.entities.Attachements;
import bank.online.entities.CarteBancaire;
import bank.online.entities.CategorieCredit;
import bank.online.entities.Credit;
import bank.online.entities.DomaineAvancement;
import bank.online.entities.PaiementCredit;
import bank.online.entities.StatusCredit;
import bank.online.entities.TypeSimulation;
import bank.online.entities.User;
import bank.online.repositories.AttachmentRepository;
import bank.online.repositories.CarteBancaireRepository;
import bank.online.repositories.CreditRepository;
import bank.online.repositories.DomainAvancementRepository;
import bank.online.repositories.PaiementRepository;
import bank.online.repositories.ProcessusRepository;
import bank.online.repositories.ProduitImmobilierRepository;
import bank.online.repositories.ProjetImmobilierRepository;
import bank.online.repositories.TarifCarteBancaireRepository;
import bank.online.repositories.UserRepository;

@Service
public class CreditServicesImpl implements ICreditServices{

    private static final Logger log = Logger.getLogger(FileStorageServicesImpl.class);
	
	private final Path root = Paths.get("src/main/resources/uploads/credits");
	
	@Value("${app.interest}")
	private Float interest;
	
	@Value("${app.tauxEndettement}")
	private Float tauxEndettement;
	
	@Autowired
	CreditRepository creditRepo;
	
	@Autowired
	TarifCarteBancaireRepository tarifRepo;
	
	@Autowired
	UserRepository userRepo;
	
	@Autowired
	AttachmentRepository fileRepo;
	
	@Autowired
	private CarteBancaireRepository carteRepo;
	
	@Autowired
	INotificationServices notificationServe;
	
	@Autowired
	PaiementRepository paiementRepo;
	
	@Autowired
	ProjetImmobilierRepository projImmoRepo;
	
	@Autowired
	ProduitImmobilierRepository prodImmoRepo;
	
	@Autowired
	DomainAvancementRepository domainAvRepo;
	
	@Autowired
	ProcessusRepository processRepo;
	
	@Transactional
	public Credit addToCarteBancaire(Credit credit, Long idUser,String carteNumber) {
		credit.setDateAjout(new Date());
		User user = userRepo.findById(idUser).orElse(null);
		Optional<CarteBancaire> carteOptional = carteRepo.getCardByNumber(carteNumber,idUser);
		
		if(carteOptional.isPresent() && !credit.getPaiements().isEmpty()) {
			CarteBancaire carte = carteOptional.get();
			for (PaiementCredit paiement : credit.getPaiements()) {
				paiement.setDateAjout(new Date());
				paiement.setCredit(credit);
				paiementRepo.save(paiement);
			}
			carte.getTypeCarte().getTarif().setProvision( carte.getTypeCarte().getTarif().getProvision() + credit.getMontantDemande());
			carte.setDateModification(new Date());
			carte.getTypeCarte().getTarif().setDateModification(new Date());
			tarifRepo.save(carte.getTypeCarte().getTarif());
			carteRepo.save(carte);
		}
		credit.setEmprunteur(user);
		credit.setInUse(true);
		return creditRepo.save(credit);
	}

	@Override
	public Credit simulerCreditConsommation(Credit credit) {
		if(credit.getCategorie().equals(CategorieCredit.CONSOMMATION)) {
			
			switch (credit.getTypeSimulation()) {
			case CALCUL_CAPACITE_EMPRUNT:
				credit = this.calculerCapaciteEmprunt(credit);
				break;
			case CALCUL_ECHEANCE:
				credit = this.CalculerEcheance(credit);
				break;
			default:
				credit = this.CalculerMensualiteCst(credit);
				break;
			}
			
			if(credit.getDuree() !=null) {
				credit.setEcheance(this.getNewDateByAddingMonth(credit.getDuree()));
			}
			
			if(credit.getTypeSimulation().equals(TypeSimulation.CALCUL_CAPACITE_EMPRUNT)) {
				Credit newCredit = new Credit(credit.getCategorie(), credit.getTypeSimulation(), 
						credit.getMontantDemande(), credit.getMontantMensuel(), credit.getMontantTransaction(), 
						credit.getDuree(), credit.getEcheance(), credit.getDescription(),credit.getPaiements(),credit.getStatus(),credit.getApportPersonnel(),
						credit.getSalaireNetPersonnel(),credit.getPrimeAnnuelle(),credit.getAutresRevenus(),credit.getEstAccorde());
				return newCredit;
			}else {
				Credit newCredit = new Credit(credit.getCategorie(), credit.getTypeSimulation(), 
						credit.getMontantDemande(), credit.getMontantMensuel(), credit.getMontantTransaction(), 
						credit.getDuree(), credit.getEcheance(), credit.getDescription(),credit.getPaiements(),credit.getStatus(),credit.getEstAccorde());
				return newCredit;
			}
			
		}
		return null;
	}
	
	private Credit CalculerEcheance(Credit credit) {
		List<PaiementCredit> paiements = new ArrayList<PaiementCredit>();
		float cout = 0;
		
		if(credit.getMontantDemande() != null && credit.getMontantMensuel()!= null && credit.getMontantDemande() >= credit.getMontantMensuel()) {
			int duree = 1;
			float interestPaye,amortissement,newCapital;
			interestPaye = (float)(credit.getMontantDemande() * interest)/(100);
			amortissement = (float)credit.getMontantMensuel() - (float)interestPaye;
			newCapital = (float)credit.getMontantDemande() - (float)amortissement;
			PaiementCredit paiement = new PaiementCredit(credit.getMontantDemande(), interestPaye, amortissement, credit.getMontantMensuel(), true, this.getNewDateByAddingMonth(1));
			paiements.add(paiement);
			
			while(amortissement < newCapital ) {
				interestPaye = (float)(newCapital * interest)/(100);
				amortissement = (float)credit.getMontantMensuel() - (float)interestPaye;
				newCapital = (float)newCapital - (float)amortissement;
				duree +=1;
				PaiementCredit newPaiement = new PaiementCredit(newCapital, interestPaye, amortissement, credit.getMontantMensuel(), true, this.getNewDateByAddingMonth(duree));
				paiements.add(newPaiement);
			}
			credit.setEstAccorde(true);
			credit.setDuree(duree);
			
		}
		
		credit.setPaiements(paiements);
		for (PaiementCredit paiementCredit : credit.getPaiements()) {
			cout += paiementCredit.getInteret();	
		}
		if(cout > 5000) {
			credit.setStatus(StatusCredit.hight);
		}else {
			credit.setStatus(StatusCredit.low);
		}
		credit.setMontantTransaction(cout);
		
		return credit;
	}
	//calculer une nouvelle date en ajouter des mois 
	private Date getNewDateByAddingMonth(int nombreMois) {
		Date newDate = null;
		
		Calendar cal = Calendar.getInstance(); 
		cal.add(Calendar.MONTH, nombreMois);
		
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd"); 
		//Date après avoir ajouté les années à la date indiquée
		String echeance = sdf.format(cal.getTime());
		try {
			newDate = sdf.parse(echeance);
		} catch (ParseException e) {
			e.printStackTrace();
		}
		return newDate;
	}
	
	private Credit CalculerMensualiteCst(Credit credit) {
		List<PaiementCredit> paiements = new ArrayList<PaiementCredit>();
		float cout = 0;
		
		if(credit.getMontantDemande() != null && credit.getDuree()!= null) {
			float capitalInvesti = credit.getMontantDemande();
			float i = (float) interest/100;
			int puissance = -credit.getDuree();
			float mensualite = (float) ((capitalInvesti * i)/(1 - Math.pow((float)(1+i),puissance)));
			
			for (int j = 1; j < credit.getDuree()+1; j++) {
				
				float restant_du = capitalInvesti;
				float interest_paye =(float) (restant_du * i);
				
				float amortissement = mensualite - interest_paye;
				
				capitalInvesti = (float) restant_du - (float)amortissement;
				
				PaiementCredit paiement = new PaiementCredit(restant_du, interest_paye, amortissement, mensualite, true, this.getNewDateByAddingMonth(j));
				paiements.add(paiement);
			}
			
			credit.setEstAccorde(true);
			credit.setMontantMensuel(mensualite);
		}
		
		credit.setPaiements(paiements);
		for (PaiementCredit paiementCredit : credit.getPaiements()) {
			cout += paiementCredit.getInteret();	
		}
		if(cout > 5000) {
			credit.setStatus(StatusCredit.hight);
		}else {
			credit.setStatus(StatusCredit.low);
		}
		credit.setMontantTransaction(cout);
		
		return credit;
	}

	@Override
	public int rembourserCreditParCarteBancaire(Long idEmprunteur,Long idCredit,String numeroCarte,Long idPaiement){
		
		int result = -1;
		Optional<CarteBancaire> carteOptional = carteRepo.getCardByNumber(numeroCarte,idEmprunteur);
		Optional<Credit> creditOptional = creditRepo.findById(idCredit);
		Optional<PaiementCredit> paieOptional = paiementRepo.findById(idPaiement);
		
		if(carteOptional.isPresent() && creditOptional.isPresent() && paieOptional.isPresent()) {
			CarteBancaire carte = carteOptional.get();
			PaiementCredit paiement = paieOptional.get();
		
			if(creditOptional.get().getPaiements().contains(paiement) && Boolean.TRUE.equals(paiement.getARembourse())) {
				paiement.setCredit(creditOptional.get());
				result = this.payerParCarteBancaire(carte, paiement);
			}else {
				result = -3;
			}
		}else {
			result = 1;
		}
		return result;
	}
	
	private int payerParCarteBancaire(CarteBancaire carte,PaiementCredit paiement) {
		int result = -1;
		if(carte.getTypeCarte().getTarif().getProvision() > paiement.getMensualite()) {
			carte.getTypeCarte().getTarif().setProvision(carte.getTypeCarte().getTarif().getProvision() -paiement.getMensualite());
			carte.setDateModification(new Date());
			carte.getTypeCarte().getTarif().setDateModification(new Date());
			tarifRepo.save(carte.getTypeCarte().getTarif());
			carteRepo.save(carte);
			
			paiement.setDateRemboursement(new Date());
			paiement.setARembourse(false);
			paiementRepo.save(paiement);
			result = 0;
		}else {
			result = -2;
		}
		return result;
	}
	
	@Scheduled(cron = "*/30 * * * * *")
	public void updateCredits() {
		List<Credit> allCredits = creditRepo.findAll();
		
		for (Credit credit : allCredits) {
			for (int i=0 ; i<credit.getPaiements().size();i++) {
				if(credit.getPaiements().get(i).getDateRemboursement() !=null) {
					credit.setRembourse(true);
				}else {
					credit.setRembourse(false);
				}
				credit.setDateModification(new Date());
				creditRepo.save(credit);
			}
		}
	}

	@Override
	public Credit simulerCreditImmobilier(Credit credit,Long idUser) {
		
		Optional<User> userOptional = userRepo.findById(idUser);
		
		if(userOptional.isPresent()) {
			credit.getProjet().getProduit().setDateAjout(new Date());
			
			if(credit.getProjet().getAvancements() !=null) {
				for (DomaineAvancement stage : credit.getProjet().getAvancements()) {
					stage.setDateAjout(new Date());
					domainAvRepo.save(stage);
				}
			}
			credit.getProjet().setDateAjout(new Date());
			
			
			credit.setEmprunteur(userOptional.get());
			credit.setDateAjout(new Date());
			credit.setEstAccorde(false);
			
			prodImmoRepo.save(credit.getProjet().getProduit());
			projImmoRepo.save(credit.getProjet());
			
		
			Credit newCredit = creditRepo.save(credit);
		
			return newCredit;
		}
		return new Credit();
	}

	private Credit calculerCapaciteEmprunt(Credit credit) {
		
		if(credit.getMontantMensuel() !=null && credit.getApportPersonnel()!= null && credit.getSalaireNetPersonnel() !=null) {
			float revenus =0;
			revenus += credit.getSalaireNetPersonnel();
			if(credit.getPrimeAnnuelle() !=null) {
				revenus += credit.getPrimeAnnuelle();
			}
			
			if(credit.getAutresRevenus() !=null) {
				revenus += credit.getAutresRevenus();
			}
			
			float richesse = revenus - (credit.getApportPersonnel() + credit.getMontantMensuel());
			float capaciteEmprunt = (float) (richesse * tauxEndettement) /(100);
			
			if(credit.getMontantDemande() == null) {
				credit.setMontantDemande(capaciteEmprunt);
			}
			
			credit = this.CalculerEcheance(credit);
		}
		return credit;
	}

	@Transactional
	public int accorderCreditImmo(Long idCredit) {
		Optional<Credit> creditOptional = creditRepo.findById(idCredit);
		
		if(creditOptional.isPresent()) {
			Credit credit =creditOptional.get();
	
			credit.setDateModification(new Date());
			
			credit = this.CalculerEcheance(credit);
			
			if(credit.getDuree() !=null) {
				credit.setEcheance(this.getNewDateByAddingMonth(credit.getDuree()));
			}
			
			credit.setInUse(false);
			Credit newCredit = creditRepo.save(credit);
			if(!credit.getPaiements().isEmpty()) {
				for (PaiementCredit paiement : credit.getPaiements()) {
					paiement.setDateAjout(new Date());
					paiement.setCredit(newCredit);
					paiementRepo.save(paiement);
				}
			}
			try {
				notificationServe.notifiyForcreditConfirmation(newCredit, newCredit.getEmprunteur());
			} catch (MessagingException e) {
				e.printStackTrace();
			}
			return 0;
		}
		return -1;
	}
	
	@Override
	  public void save(MultipartFile file,Long idCredit) {
		List<Attachements> attachements = new ArrayList<Attachements>();
		Optional<Credit> creditOptional = creditRepo.findById(idCredit);
		
		if(creditOptional.isPresent()) {
			Credit credit = creditOptional.get();
			try {
		    	Attachements fileAtts = new Attachements();
		    	fileAtts.setName(file.getOriginalFilename());
		    	fileAtts.setSize((int)file.getSize());
		    	
		    	attachements.add(fileRepo.save(fileAtts));
		      Files.copy(file.getInputStream(), this.root.resolve(file.getOriginalFilename()));
		    } catch (Exception e) {
		    	log.debug("Could not store the file. Error: " + e.getMessage());
		    }
			credit.setFichiers(attachements);
			credit.setDateModification(new Date());
			creditRepo.save(credit);
		}
	  }
	
	@Override
	  public Resource load(String filename) {
	    try {
	    	Path file =  root.resolve(filename);
	 
	      Resource resource = new UrlResource(file.toUri());
	      if (resource.exists() || resource.isReadable()) {
	        return resource;
	      } else {
	    	  log.info("Could not read the file! :"+filename);
	    	 return null;
	      }
	    } catch (MalformedURLException e) {
	    	log.debug(e);
	      return null;
	    }
	    
	  }

	@Override
	public void useCredit(Credit credit, Long idUser, String carteNumber) {
		credit.setDateAjout(new Date());
		User user = userRepo.findById(idUser).orElse(null);
		Optional<CarteBancaire> carteOptional = carteRepo.getCardByNumber(carteNumber,idUser);
		
		if(carteOptional.isPresent() && !credit.getPaiements().isEmpty()) {
			CarteBancaire carte = carteOptional.get();
			carte.getTypeCarte().getTarif().setProvision( carte.getTypeCarte().getTarif().getProvision() + credit.getMontantDemande());
			carte.setDateModification(new Date());
			carte.getTypeCarte().getTarif().setDateModification(new Date());
			
			credit.setInUse(true);
			tarifRepo.save(carte.getTypeCarte().getTarif());
			carteRepo.save(carte);
			
			creditRepo.save(credit);
		}
		credit.setEmprunteur(user);
		credit.setInUse(true);
		creditRepo.save(credit);
	}
}
