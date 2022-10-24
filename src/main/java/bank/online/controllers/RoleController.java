package bank.online.controllers;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import bank.online.entities.Role;
import bank.online.payload.response.MessageResponse;
import bank.online.repositories.RoleRepository;

@RestController
@RequestMapping("roles")
public class RoleController {

	@Autowired
	RoleRepository roleRepo;
	
	@GetMapping("find-all")
	@ResponseBody
	public List<Role> findAll(){
		return roleRepo.findAll();
	}
	
	@GetMapping("find-all-for-rh")
	@ResponseBody
	public List<Role> findAllFORRH(){
		return roleRepo.findAllRoleForRH();
	}
	
	@GetMapping("find-all-for-directoire")
	@ResponseBody
	public List<Role> findAllFORDIRECTOIRE(){
		return roleRepo.findAllRoleForDirectoir();
	}
	
	@GetMapping("/find/{id}")
	@ResponseBody
	public ResponseEntity<Object> find(@PathVariable("id") Integer id) {
		
		if(Boolean.FALSE.equals(roleRepo.existsById(id))) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("Le role recherché n'existe pas!"));
		}
		return ResponseEntity.ok().body(roleRepo.findById(id));
	}
	
	@PostMapping("add")
	@ResponseBody
	public ResponseEntity<Object> add(@RequestBody Role role){
		if(Boolean.TRUE.equals(roleRepo.existsByName(role.getName().name()))) {
			return ResponseEntity.status(HttpStatus.NOT_ACCEPTABLE).body(new MessageResponse("Le role existe deja!"));
		}
		
		return ResponseEntity.ok().body(roleRepo.save(role));
	}
}
