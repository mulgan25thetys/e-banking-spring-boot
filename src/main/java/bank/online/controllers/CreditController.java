package bank.online.controllers;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import bank.online.entities.CarteBancaire;
import bank.online.entities.Credit;
import bank.online.entities.ERole;
import bank.online.entities.PaiementCredit;
import bank.online.entities.User;
import bank.online.payload.response.MessageResponse;
import bank.online.repositories.CarteBancaireRepository;
import bank.online.repositories.CreditRepository;
import bank.online.repositories.PaiementRepository;
import bank.online.repositories.UserRepository;
import bank.online.services.ICreditServices;
import bank.online.services.IPaiementCreditServices;

@RestController
@RequestMapping("credits")
public class CreditController {

	@Value("${app.interest}")
	private Float interest;
	@Autowired
	CreditRepository creditRepo;
	
	@Autowired
	ICreditServices creditService;
	
	@Autowired
	CarteBancaireRepository carteRepo;
	
	@Autowired
	UserRepository userRepo;
	
	@Autowired
	IPaiementCreditServices payCreditService;
	
	@Autowired
	PaiementRepository paiementRepo;
	
	@GetMapping("find-all")
	@ResponseBody
	public List<Credit> findAll(){
		return creditRepo.findAll();
	}
	
	@GetMapping("find/{id}")
	@ResponseBody
	public ResponseEntity<Object> find(@PathVariable("id") Long id){
		
		if(Boolean.FALSE.equals(creditRepo.existsById(id))) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("Le credit recherché est introuvable!"));
		}
		
		return ResponseEntity.ok().body(creditRepo.findById(id));
	}
	
	@PostMapping("simulate-credit-conso")
	@ResponseBody
	public ResponseEntity<Object> simulateCreditConso(@Valid @RequestBody Credit credit){
		
		return ResponseEntity.ok().body(creditService.simulerCreditConsommation(credit));
	}
	
	@PostMapping("simulate-credit-immo")
	@ResponseBody
	public ResponseEntity<Object> simulateCreditImmo(){
		return null;
	}
	
	@PostMapping("add-credit-to-card/{idUser}/{numeroCarte}")
	@ResponseBody
	public ResponseEntity<Object> addCredit(@RequestBody Credit credit,@PathVariable("idUser") Long idUser,@PathVariable("numeroCarte") String numeroCarte){
		if(Boolean.FALSE.equals(userRepo.existsById(idUser))) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("L'utilisateur recherché est introuvable!"));
		}
		
		Optional<User> user = userRepo.findById(idUser);
		if(user.isPresent() && !user.get().getRole().getName().equals(ERole.ROLE_CLIENT)) {
			
			return ResponseEntity.badRequest().body(new MessageResponse("L'opération a échouée,Veuillez vous abonner!"));
		}
		
		Optional<CarteBancaire> carteOptional= carteRepo.getCardByNumber(numeroCarte, idUser);
		if(!carteOptional.isPresent()) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new MessageResponse("Attention! La carte indiqué est introuvable."));
		}
		
		if(user.isPresent() && user.get().getCredit() !=null) {
			
			if(user.get().getCredit().getRembourse() == null || user.get().getCredit().getRembourse() == false) {
				return ResponseEntity.badRequest().body(new MessageResponse("L'opération a échouée,Vous avez un credit en cours!"));	
			}
		}
		return ResponseEntity.ok().body(creditService.addToCarteBancaire(credit,idUser,numeroCarte));
	}

	@GetMapping("get-paiements/{idCredit}")
	@ResponseBody
	public ResponseEntity<Object> getPaiemnts(@PathVariable("idCredit") Long idCredit){
		
		Optional<Credit> existedCredit = creditRepo.findById(idCredit);
		if(existedCredit.isPresent()) {
			List<PaiementCredit> paiements = existedCredit.get().getPaiements();
			return ResponseEntity.ok().body(paiements);
		}
		return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("Le credit recherché est introuvable!"));
	}
	
	@GetMapping("find-by-user/{id}")
	@ResponseBody
	public ResponseEntity<Object> findByUser(@PathVariable("id") Long id){
		
		if(Boolean.FALSE.equals(userRepo.existsById(id))) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("L'utilisateur recherché est introuvable!"));
		}
		
		Optional<User> user = userRepo.findById(id);
		if(user.isPresent()) {
			Credit credit = user.get().getCredit();
			return ResponseEntity.ok().body(credit);
		}
		return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new MessageResponse("Une erreur s'est produite veuillez ressayer!"));
	}
	
	@GetMapping("get-mensuality-by-user/{id}")
	@ResponseBody
	public ResponseEntity<Object> getMensuality(@PathVariable("id") Long id){
		
		if(Boolean.FALSE.equals(userRepo.existsById(id))) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("L'utilisateur recherché est introuvable!"));
		}
		Date today = new Date();
		Optional<PaiementCredit> paiement = paiementRepo.getMonthlyMensualityToPay(today, id);
		if(paiement.isPresent()) {
			return ResponseEntity.ok().body(paiement.get());
		}
		return ResponseEntity.status(HttpStatus.ACCEPTED).body(new MessageResponse("Aucun versement à effectuer!"));
	}
	
	@PutMapping("rembourser-credit-by-user-by-card/{idUser}/{idCredit}/{idpai}/{numeroCarte}")
	@ResponseBody
	public ResponseEntity<Object> rembourserCredit(@PathVariable("idUser") Long id,@PathVariable("numeroCarte") 
	String numeroCarte,@PathVariable("idpai") Long idpai,@PathVariable("idCredit") Long idCredit){
		
		if(Boolean.FALSE.equals(userRepo.existsById(id))) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("L'utilisateur recherché est introuvable!"));
		}
		
		Optional<Credit> creditOptional= creditRepo.findById(idCredit);
		if(!creditOptional.isPresent()) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new MessageResponse("Aucun credit en cours à rembourser!"));
		}
		
		Optional<CarteBancaire> carteOptional= carteRepo.getCardByNumber(numeroCarte, id);
		if(!carteOptional.isPresent()) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new MessageResponse("La carte indiqué est introuvable ou numéro invalide!"));
		}
		
		int verif = creditService.rembourserCreditParCarteBancaire(id,idCredit, numeroCarte, idpai);
		
		if(verif == -2) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new MessageResponse("Vous etes en manque de provision sur cette carte!"));
		}
		
		if(verif == -3) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new MessageResponse("Merci! Le paiement a été deja effectué!"));
		}
		
		if(verif == 1) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new MessageResponse("Le numéro de la carte est invalide! ou le remboursement est achevé"));
		}
		
		if(verif == -1) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(new MessageResponse("L'opération a échouée, Veuillez ressayer!"));
		}
		
		return ResponseEntity.ok().body(new MessageResponse("Votre remboursement a été bien réalisé!"));
	}
}
