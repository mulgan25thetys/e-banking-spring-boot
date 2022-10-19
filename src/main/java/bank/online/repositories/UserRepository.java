package bank.online.repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import bank.online.entities.User;

@Repository
public interface UserRepository extends JpaRepository<User, Long> {
	Optional<User> findByUsername(String username);
	
	@Query(value = "SELECT * FROM user WHERE email =:value OR username=:value",nativeQuery = true)
	User findByUsernameOrEmail(@Param("value") String value);
	
	@Query(value = "SELECT * FROM user u INNER JOIN role r on u.role_id = r.id "
			+ "WHERE r.name IN ('ROLE_GESTIONNAIRE_CLIENTELE','ROLE_CONSEILLER_CLIENTELE',"
			+ "'ROLE_PERSONNEL_FINANCIER','ROLE_PERSONNEL_RH')",nativeQuery = true)
	List<User> findAllPersonnals();
	
	@Query(value = "SELECT * FROM user u INNER JOIN role r on u.role_id = r.id "
			+ "WHERE r.name = 'ROLE_CLIENT'",nativeQuery = true)
	List<User> findAllClients();
	
	@Query(value = "SELECT * FROM user u INNER JOIN user_carte_bancaires ucb "
			+ "ON u.id = ucb.user_id INNER JOIN carte_bancaire cb "
			+ "ON cb.id_carteb = ucb.carte_bancaires_id_carteb  WHERE cb.id_carteb = :idCard",nativeQuery = true)
	List<User> getUsersByCards(@Param("idCard") Long idCard);
	
	Boolean existsByUsername(String username);
	Boolean existsByEmail(String email);

}