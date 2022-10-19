package bank.online.controllers;

import java.util.List;
import java.util.Optional;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import bank.online.entities.Devises;
import bank.online.payload.response.MessageResponse;
import bank.online.repositories.DevisesRepository;
import bank.online.services.IDevisesServices;

@RestController
@RequestMapping("devises")
public class DeviseController {

	@Autowired
	IDevisesServices deviseServise;
	
	@Autowired
	DevisesRepository deviseRepo;
	
	@GetMapping("/find-all")
	@ResponseBody
	public List<Devises> getAllDevises() {
		return deviseServise.findAll();
	}
	
	@SuppressWarnings("all")
	@PostMapping("/add-devise")
	@ResponseBody
	public ResponseEntity<Object> addDevise(@Valid @RequestBody Devises devise){
		if(Boolean.TRUE.equals(deviseRepo.existsByNom(devise.getNom()))){
			return ResponseEntity.badRequest().body(new MessageResponse("Le nom du devise est deja existant!"));
		}
		
		if(Boolean.TRUE.equals(deviseRepo.existsByIndice(devise.getIndice()))){
			return ResponseEntity.badRequest().body(new MessageResponse("L'indice du devise est deja existant!"));
		}
		
		return ResponseEntity.ok().body(deviseServise.addDevise(devise));
	}
	
	@SuppressWarnings("all")
	@PutMapping("/edit-devise")
	@ResponseBody
	public ResponseEntity<Object> editDevise(@Valid @RequestBody Devises devise){
		
		if(Boolean.TRUE.equals(deviseRepo.existsByNom(devise.getNom()))){
			Optional<Devises> existedDevise = deviseRepo.findByNom(devise.getNom());
			if(existedDevise.isPresent() && existedDevise.get().getIdDevise() != devise.getIdDevise()) {
				return ResponseEntity.badRequest().body(new MessageResponse("Le nom du devise est deja pris!"));
			}
		}
		
		if(Boolean.TRUE.equals(deviseRepo.existsByIndice(devise.getIndice()))){
			Optional<Devises> existedDevise = deviseRepo.findByIndice(devise.getIndice());
			if(existedDevise.isPresent() && existedDevise.get().getIdDevise() != devise.getIdDevise()) {
				return ResponseEntity.badRequest().body(new MessageResponse("L'indice du devise est deja pris!"));
			}
		}
		
		return ResponseEntity.ok().body(deviseServise.editDevise(devise));
	}
	
	@GetMapping("/find/{id}")
	@ResponseBody
	public ResponseEntity<Object> findDevise(@PathVariable("id") Long id){
		if(Boolean.FALSE.equals(deviseRepo.existsById(id))){
			return ResponseEntity.badRequest().body(new MessageResponse("La devise recherchée n'existe pas!"));
		}
		
		return ResponseEntity.ok().body(deviseServise.getDevise(id));
	}
	
	@DeleteMapping("/delete/{id}")
	@ResponseBody
	public ResponseEntity<Object> deleteDevise(@PathVariable("id") Long id){
		if(Boolean.FALSE.equals(deviseRepo.existsById(id))){
			return ResponseEntity.badRequest().body(new MessageResponse("La devise est introuvable!"));
		}
		Optional<Devises> devise = deviseRepo.findById(id);
		String name = "";
		if(devise.isPresent()) {
			name = devise.get().getNom() +" ("+devise.get().getIndice()+")";
		}
		deviseServise.removeDevise(id);
		return ResponseEntity.ok().body(new MessageResponse("La devise "+name+" a été supprimée"));
	}
}
