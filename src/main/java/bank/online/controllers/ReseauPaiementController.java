package bank.online.controllers;

import java.util.List;
import java.util.Optional;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import bank.online.entities.ReseauPaiement;
import bank.online.payload.response.MessageResponse;
import bank.online.repositories.ReseauPaiementRepository;
import bank.online.services.IReseauPaiementServices;

@CrossOrigin(origins = "*", maxAge = 3600)
@RestController
@RequestMapping("reseau-paiements")
public class ReseauPaiementController {

	@Autowired
	IReseauPaiementServices reseauPayService;
	
	@Autowired
	ReseauPaiementRepository reseauRepo;
	
	@GetMapping("/find-all")
	@ResponseBody
	public List<ReseauPaiement> getAllRseauPay() {
		return reseauPayService.findAll();
	}
	
	@SuppressWarnings("all")
	@PostMapping("/add-reseau-paiement")
	@ResponseBody
	public ResponseEntity<Object> addReseauPay(@Valid @RequestBody ReseauPaiement reseau){
		if(Boolean.TRUE.equals(reseauRepo.existsByNom(reseau.getNom()))){
			return ResponseEntity.badRequest().body(new MessageResponse("Le nom de ce réseau de paiement est deja existant!"));
		}
		
		return ResponseEntity.ok().body(reseauPayService.addReseauPay(reseau));
	}
	
	@SuppressWarnings("all")
	@PutMapping("/edit-reseau-paiement")
	@ResponseBody
	public ResponseEntity<Object> editReseau(@Valid @RequestBody ReseauPaiement reseau){
		
		if(Boolean.TRUE.equals(reseauRepo.existsByNom(reseau.getNom()))){
			Optional<ReseauPaiement> existedReseau = reseauRepo.findByNom(reseau.getNom());
			if(existedReseau.isPresent() && existedReseau.get().getIdReseau() != reseau.getIdReseau()) {
				return ResponseEntity.badRequest().body(new MessageResponse("Le nom du reseau de paiement est deja pris!"));
			}
		}
		
		return ResponseEntity.ok().body(reseauPayService.editReseauPay(reseau));
	}
	
	@GetMapping("/find/{id}")
	@ResponseBody
	public ResponseEntity<Object> find(@PathVariable("id") Long id){
		if(Boolean.FALSE.equals(reseauRepo.existsById(id))){
			return ResponseEntity.badRequest().body(new MessageResponse("Le reseau de paiement recherché n'existe pas!"));
		}
		
		return ResponseEntity.ok().body(reseauPayService.getReseauPaiement(id));
	}
	
	@DeleteMapping("/delete/{id}")
	@ResponseBody
	public ResponseEntity<Object> delete(@PathVariable("id") Long id){
		if(Boolean.FALSE.equals(reseauRepo.existsById(id))){
			return ResponseEntity.badRequest().body(new MessageResponse("Le reseau de paiement est introuvable!"));
		}
		Optional<ReseauPaiement> reseau = reseauRepo.findById(id);
		String name = "";
		if(reseau.isPresent()) {
			name = reseau.get().getNom() ;
		}
		reseauPayService.deleteReseauPay(id);
		return ResponseEntity.ok().body(new MessageResponse("Le reseau de paiement "+name+" a été supprimé"));
	}
}
