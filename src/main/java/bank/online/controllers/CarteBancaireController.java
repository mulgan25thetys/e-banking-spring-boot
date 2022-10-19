package bank.online.controllers;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import bank.online.entities.CarteBancaire;
import bank.online.payload.response.MessageResponse;
import bank.online.repositories.CarteBancaireRepository;
import bank.online.repositories.UserRepository;
import bank.online.services.ICarteBancaireServices;

@RestController
@RequestMapping("carte-bancaires")
public class CarteBancaireController {

	@Autowired
	ICarteBancaireServices carteServices;
	
	@Autowired
	CarteBancaireRepository carteRepo;
	
	@Autowired
	UserRepository userRepo;
	
	@GetMapping("find-all")
	@ResponseBody
	public List<CarteBancaire> findAll(){
		return carteServices.findAll();
	}
	
	@GetMapping("find-non-subscribed-cards")
	@ResponseBody
	public List<CarteBancaire> findAllUnscribeCards(){
		return carteRepo.getNonSubscribed();
	}
	
	@GetMapping("find/{id}")
	@ResponseBody
	public ResponseEntity<Object> find(@PathVariable("id") Long id) {
		
		if(Boolean.FALSE.equals(carteRepo.existsById(id))) {
			return ResponseEntity.badRequest().body(new MessageResponse("La carte bancaire est introuvable"));
		}
		
		return ResponseEntity.ok().body(carteServices.getCarteBancaire(id));
	}
	
	@DeleteMapping("delete/{id}")
	@ResponseBody
	public ResponseEntity<Object> delete(@PathVariable("id") Long id){
		if(Boolean.FALSE.equals(carteRepo.existsById(id))) {
			return ResponseEntity.badRequest().body(new MessageResponse("Nous ne pouvons retrouver cette carte bancaire"));
		}
		
		Optional<CarteBancaire> optCard = carteRepo.findById(id);
		String cardDetail = "";
		if(optCard.isPresent()) {
			cardDetail = optCard.get().getTypeCarte().getNom()+ " de numero: "+optCard.get().getNumero();
		}
		carteServices.removeCarte(optCard.get().getIdCarteB());
		return ResponseEntity.ok().body(new MessageResponse("Un carte bancaire "+cardDetail+" a été supprimée"));
	}
	
	@PostMapping("add")
	@ResponseBody
	public ResponseEntity<Object> add(@RequestBody CarteBancaire carte){
		return ResponseEntity.ok().body(carteServices.addCarteBancaire(carte));
	}
	
	@PutMapping("edit")
	@ResponseBody
	public ResponseEntity<Object> edit(@RequestBody CarteBancaire carte){
		return ResponseEntity.ok().body(carteServices.editCarteBancaire(carte));
	}
	
	@PreAuthorize("hasAnyRole('ROLE_CLIENT')")
	@PutMapping("souscrire-carte-bancaire/{idC}/{idU}")
	@ResponseBody
	public ResponseEntity<Object> souscrir(@PathVariable("idU") Long idUser,@PathVariable("idC") Long idCard){
		Integer verif= carteServices.souscrieCarteBancaire(idCard, idUser);
		
		if(verif == 1) {
			return ResponseEntity.badRequest().body(new MessageResponse("Merci! vous avez deja souscrit à cette carte "));
		}
		if(verif == 2) {
			return ResponseEntity.badRequest().body(new MessageResponse("Désolé! la carte est deja souscrite! "));
		}
		
		if(verif == -1) {
			return ResponseEntity.badRequest().body(new MessageResponse("Une erreur s'est produite veuillez réessayer "));
		}
		
		return ResponseEntity.ok().body(new MessageResponse("Votre subscription a été pris en compte"));
	}
	
	@PutMapping("annuler-souscrire-carte-bancaire/{idC}/{idU}")
	@ResponseBody
	public ResponseEntity<Object> annulerSouscrir(@PathVariable("idU") Long idUser,@PathVariable("idC") Long idCard){
		Integer verif= carteServices.cancelSubscriptionCarteBancaire(idCard, idUser);
	
		if(verif == -1) {
			return ResponseEntity.badRequest().body(new MessageResponse("Une erreur s'est produite veuillez réessayer! "));
		}
		
		return ResponseEntity.ok().body(new MessageResponse("Annulation de la subscription a été pris en compte"));
	}
	
	@PutMapping("resilier-contrat/{idC}/{idU}")
	@ResponseBody
	public ResponseEntity<Object> resilier(@PathVariable("idU") Long idUser,@PathVariable("idC") Long idCard){
		Integer verif= carteServices.resilierContratSubsciption(idCard, idUser);
		
		if(verif == 1) {
			return ResponseEntity.badRequest().body(new MessageResponse("Désolé! vous n'etes pas proprietaire! "));
		}
		
		if(verif == -1) {
			return ResponseEntity.badRequest().body(new MessageResponse("Une erreur s'est produite veuillez réessayer!! "));
		}
		
		return ResponseEntity.ok().body(new MessageResponse("Votre résiliation a été pris en compte"));
	}
	
	@PutMapping("ajouter-contrat/{idC}/{idContrat}/{idU}")
	@ResponseBody
	public ResponseEntity<Object> ajouterContrat(@PathVariable("idU") Long idUser,@PathVariable("idContrat") Long idContrat,@PathVariable("idC") Long idCard){
		Integer verif= carteServices.ajouterUnContratAssurance(idCard,idContrat, idUser);
		
		if(verif == 1) {
			return ResponseEntity.badRequest().body(new MessageResponse("Désolé! vous n'etes pas proprietaire de la carte! "));
		}
		
		if(verif == -1) {
			return ResponseEntity.badRequest().body(new MessageResponse("Une erreur s'est produite veuillez réessayer!!! "));
		}
		
		return ResponseEntity.ok().body(new MessageResponse("Vous venez de souscrire à un contrat d'assurance!"));
	}
	
	@GetMapping("get-by-user/{id}")
	@ResponseBody
	public ResponseEntity<Object> getByUser(@PathVariable("id") Long id) {
		
		if(Boolean.FALSE.equals(userRepo.existsById(id))) {
			return ResponseEntity.badRequest().body(new MessageResponse("L'utilisateur est introuvale!"));
		}
		
		return ResponseEntity.ok().body(carteServices.getCarteByUser(id));
	}
	
	@GetMapping("get-by-type/{name}")
	@ResponseBody
	public ResponseEntity<Object> getByType(@PathVariable("name") String typeName) {
		
		return ResponseEntity.ok().body(carteServices.getCarteByType(typeName));
	}
}
