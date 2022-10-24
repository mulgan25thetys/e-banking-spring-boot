package bank.online.services;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import bank.online.entities.CarteBancaire;
import bank.online.entities.CategorieCredit;
import bank.online.entities.Credit;
import bank.online.entities.PaiementCredit;
import bank.online.entities.User;
import bank.online.repositories.CarteBancaireRepository;
import bank.online.repositories.CreditRepository;
import bank.online.repositories.PaiementRepository;
import bank.online.repositories.TarifCarteBancaireRepository;
import bank.online.repositories.UserRepository;

@Service
public class CreditServicesImpl implements ICreditServices{

	@Value("${app.interest}")
	private Float interest;
	
	@Autowired
	CreditRepository creditRepo;
	
	@Autowired
	TarifCarteBancaireRepository tarifRepo;
	
	@Autowired
	UserRepository userRepo;
	
	@Autowired
	private CarteBancaireRepository carteRepo;
	
	@Autowired
	PaiementRepository paiementRepo;
	
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
		return creditRepo.save(credit);
	}

	@Override
	public Credit simulerCreditConsommation(Credit credit) {
		if(credit.getCategorie().equals(CategorieCredit.CONSOMMATION)) {
			switch (credit.getModeRemboursement()) {
			case AMORTISSEMENT_CONSTANT:
				credit = this.CalculerAmortissementCst(credit);
				break;
			case MENSUALITE_CONSTANTE:
				credit = this.CalculerMensualiteCst(credit);
				break;
			default:
				credit = this.CalculerRemboursementInfine(credit);
				break;
			}
			credit.setEcheance(this.getNewDateByAddingMonth(credit.getDuree()));
			Credit newCredit = new Credit(credit.getCategorie(), credit.getModeRemboursement(), 
					credit.getMontantDemande(), credit.getMontantMensuel(), credit.getMontantTransaction(), 
					credit.getDuree(), credit.getEcheance(), credit.getDescription(),credit.getPaiements());
			return newCredit;
		}
		return null;
	}
	
	private Credit CalculerRemboursementInfine(Credit credit) {
		List<PaiementCredit> paiements = new ArrayList<PaiementCredit>();
		float cout = 0;
		
		if(credit.getMontantDemande() != null && credit.getDuree()!= null) {
			for (int i = 1; i < credit.getDuree()+1; i++) {
				
				
				float restant_du = credit.getMontantDemande();
				float interest_paye = (restant_du * interest)/(100);
				float amortissement =0;
				Boolean aRembourse = false;
				if(i == credit.getDuree()) {
					aRembourse = true;
					amortissement = credit.getMontantDemande();
				}else {
					amortissement = (float)0;
					aRembourse = false;
				}
				float mensualite = amortissement + interest_paye;
				
				PaiementCredit paiement = new PaiementCredit(restant_du, interest_paye, amortissement, mensualite, aRembourse, this.getNewDateByAddingMonth(i));
				paiements.add(paiement);
			}
		}
		
		credit.setPaiements(paiements);
		for (PaiementCredit paiementCredit : credit.getPaiements()) {
			cout += paiementCredit.getInteret();	
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
	
	private Credit CalculerAmortissementCst(Credit credit) {
		
		List<PaiementCredit> paiements = new ArrayList<PaiementCredit>();
		float cout = 0;
		
		if(credit.getMontantDemande() != null && credit.getDuree()!= null) {
			float capitalInvesti = credit.getMontantDemande();
			float i = (float) interest/100;
		
			float amortissement = (float) capitalInvesti / credit.getDuree();
			
			for (int j = 1; j < credit.getDuree()+1; j++) {
				
				float restant_du = capitalInvesti;
				float interest_paye =(float) (restant_du * i);
				
				capitalInvesti = (float) restant_du - (float)amortissement;
				float mensualite = amortissement + interest_paye;
				
				PaiementCredit paiement = new PaiementCredit(restant_du, interest_paye, amortissement, mensualite, true, this.getNewDateByAddingMonth(j));
				paiements.add(paiement);
			}
			
			//credit.setMontantMensuel(mensualite);
		}
		
		credit.setPaiements(paiements);
		for (PaiementCredit paiementCredit : credit.getPaiements()) {
			cout += paiementCredit.getInteret();	
		}
		credit.setMontantTransaction(cout);
		
		return credit;
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
			
			credit.setMontantMensuel(mensualite);
		}
		
		credit.setPaiements(paiements);
		for (PaiementCredit paiementCredit : credit.getPaiements()) {
			cout += paiementCredit.getInteret();	
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
	
	@Scheduled(cron = "0 */1 * * * *")
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
	
	
}
