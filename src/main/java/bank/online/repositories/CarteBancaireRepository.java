package bank.online.repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import bank.online.entities.CarteBancaire;

@Repository
public interface CarteBancaireRepository extends JpaRepository<CarteBancaire, Long> {

	@Query(value = "SELECT * FROM carte_bancaire cb INNER JOIN user_carte_bancaires ucb ON ucb.carte_bancaires_id_carteb = cb.id_carteb "
			+ "INNER JOIN user u ON u.id = ucb.user_id WHERE u.id =:idUser",nativeQuery = true)
	List<CarteBancaire> getCardsByUser(@Param("idUser") Long idUser);
	
	@Query(value = "SELECT * FROM carte_bancaire cb "
			+ "INNER JOIN type_carte_bancaire tcb ON cb.id_carteb = tcb.id_type_carte "
			+ "WHERE tcb.nom =:type",nativeQuery = true)
	List<CarteBancaire> getCardsByType(@Param("type") String type);
	
	@Query(value = "SELECT * FROM carte_bancaire cb INNER JOIN  user_carte_bancaires ucb "
			+ "ON cb.id_carteb = ucb.carte_bancaires_id_carteb WHERE cb.id_carteb = :idCard",nativeQuery = true)
	Optional<CarteBancaire> getIfSubscribedCard(@Param("idCard") Long idCard);
	
	@Query(value = "SELECT * FROM carte_bancaire WHERE est_soucrite =0 OR est_soucrite is null",nativeQuery = true)
	List<CarteBancaire> getNonSubscribed();
}
