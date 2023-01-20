package bank.online.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import bank.online.entities.Chat;
import bank.online.repositories.ChatRepository;
import bank.online.repositories.UserRepository;

@CrossOrigin(origins = "*", maxAge = 3600)
@RestController
@RequestMapping("chats")
public class ChatController {

	@Autowired
	ChatRepository chatRepo;
	
	@Autowired
	UserRepository userRepo;
	
	@GetMapping("/get-user-message/{uniqueId}")
	@ResponseBody
	public ResponseEntity<Object> getUserMessage(@PathVariable("uniqueId") Long unique){
	
		return ResponseEntity.ok().body(chatRepo.getUserMessages(unique));
	}
	
	@PostMapping("add-user-message")
	@ResponseBody
	public Chat addUserMessage(@RequestBody Chat chat) {
		return chatRepo.save(chat);
	}
}

