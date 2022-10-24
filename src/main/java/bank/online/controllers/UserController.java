package bank.online.controllers;

import java.util.List;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;

import bank.online.entities.User;
import bank.online.entities.Role;
import bank.online.payload.request.LoginRequest;
import bank.online.payload.response.MessageResponse;
import bank.online.repositories.UserRepository;
import bank.online.repositories.RoleRepository;
import bank.online.services.IUserServices;

@RestController
@RequestMapping("/users")
public class UserController {
	
	@Autowired
	UserRepository userRepository;
	
	@Autowired
	RoleRepository roleRepository;
	
	@Autowired
	IUserServices userService;

	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE')")
	@GetMapping("/list-all")
	@ResponseBody
	public List<User> findAll() {
		return userService.findAll();
	}
	
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_PERSONNEL_RH','ROLE_GESTIONNAIRE_CLIENTELE','ROLE_CONSEILLER_CLIENTELE')")
	@GetMapping("/get-role-by-name/{role}")
	@ResponseBody 
	public Role getRoleByName(@PathVariable("role") String roleName) {
		return roleRepository.getRoleByName(roleName);
		
	}
	
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_GESTIONNAIRE_CLIENTELE','ROLE_CONSEILLER_CLIENTELE','ROLE_GESTIONNAIRE_PATRIMOINE')")
	@GetMapping("/list-all-clients")
	@ResponseBody
	public List<User> findAllClients() {
		return userRepository.findAllClients();
	}
	
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_PERSONNEL_RH')")
	@GetMapping("/list-all-personnals")
	@ResponseBody
	public List<User> findAllPersonnals() {
		return userRepository.findAllPersonnals();
	}

	
	@SuppressWarnings("all")
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_PERSONNEL_RH',"
			+ "'ROLE_GESTIONNAIRE_CLIENTELE','ROLE_CONSEILLER_CLIENTELE',"
			+ "'ROLE_DIRECTEUR_FINANCIER','ROLE_GESTIONNAIRE_PATRIMOINE','ROLE_DECIDEUR','ROLE_CONTROLEUR_GESTION','ROLE_EMPLOYE_CAP','ROLE_CLIENT')")
	@PutMapping("/edit-user")
	@ResponseBody
	public ResponseEntity<Object> editUser(@Valid @RequestBody User usr)
	{
		
		if (Boolean.TRUE.equals(userRepository.existsByUsername(usr.getUsername()))) {
			User u = userRepository.findByUsernameOrEmail(usr.getUsername());
			if(!u.getId().equals(usr.getId()))
			{
				return ResponseEntity
						.badRequest()
						.body(new MessageResponse("Error: Username is already taken!"));
			}
		}
		
		if(Boolean.TRUE.equals(userRepository.existsByEmail(usr.getEmail()))) {
			User u = userRepository.findByUsernameOrEmail(usr.getEmail());
			if (!u.getId().equals(usr.getId())) {
				return ResponseEntity
						.badRequest()
						.body(new MessageResponse("Error: Email is already in use!"));
			}
		}
		return ResponseEntity.ok().body(userService.editUser(usr));
	}
	
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_PERSONNEL_RH',"
			+ "'ROLE_GESTIONNAIRE_CLIENTELE','ROLE_CONSEILLER_CLIENTELE',"
			+ "'ROLE_PERSONNEL_FINANCIER','ROLE_EMPLOYE_CAP','ROLE_CLIENT')")
	@PutMapping("/edit-profile/{id}")
	@ResponseBody
	public User editProfile(@RequestParam("profile") MultipartFile profile,@PathVariable("id") Long id)
	{
		return userService.editProfile(profile,id);
	}
	
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_PERSONNEL_RH',"
			+ "'ROLE_GESTIONNAIRE_CLIENTELE','ROLE_CONSEILLER_CLIENTELE',"
			+ "'ROLE_PERSONNEL_FINANCIER','ROLE_EMPLOYE_CAP','ROLE_CLIENT')")
	@GetMapping("/get-user/{id}")
	@ResponseBody
	public ResponseEntity<Object> getUser(@PathVariable("id") Long id) {
		
		if(Boolean.FALSE.equals(userRepository.existsById(id))) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(new MessageResponse("Cannot get this User Profile"));
		}
		return ResponseEntity.ok().body(userService.getUser(id));
	}
	
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_PERSONNEL_RH',"
			+ "'ROLE_GESTIONNAIRE_CLIENTELE','ROLE_CONSEILLER_CLIENTELE',"
			+ "'ROLE_PERSONNEL_FINANCIER','ROLE_EMPLOYE_CAP','ROLE_CLIENT')")
	@GetMapping("/get-profile/{filename}")
	@ResponseBody
	public MessageResponse getProfile(@PathVariable("filename") String filename) {
		return new MessageResponse(userService.getProfile(filename));
	}
	
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_PERSONNEL_RH',"
			+ "'ROLE_GESTIONNAIRE_CLIENTELE','ROLE_CONSEILLER_CLIENTELE',"
			+ "'ROLE_PERSONNEL_FINANCIER','ROLE_EMPLOYE_CAP','ROLE_CLIENT')")
	@PutMapping("/change-password/{id}")
	@ResponseBody
	public User changePassword(@PathVariable("id") Long id,@RequestBody LoginRequest req)
	{
		return userService.changePassword(id, req.getPassword());
	}

	
	@SuppressWarnings("all")
	@PreAuthorize("hasAnyRole('ROLE_MEMBRE_DIRECTOIRE','ROLE_PERSONNEL_RH')")
	@PostMapping("/add")
	@ResponseBody
	public ResponseEntity<Object> addUser(@RequestBody User u)
	{
		if (Boolean.TRUE.equals(userRepository.existsByUsername(u.getUsername()))) {
			return ResponseEntity
					.badRequest()
					.body(new MessageResponse("Error: Username is already taken!"));
		}
		if (Boolean.TRUE.equals(userRepository.existsByEmail(u.getEmail()))) {
			return ResponseEntity
					.badRequest()
					.body(new MessageResponse("Error: Email is already in use!"));
		}
		
		return ResponseEntity.ok().body(userService.addUser(u));
	}
}
